# 05b_train_xgboost_delta.R
# Same model as 05, with one change: instead of predicting demand directly,
# it predicts the CHANGE from the same hour yesterday:
#
#     target = demand_mw - lag_24h
#     forecast = lag_24h + predicted change
#
# Why: a tree can only output averages of things it saw in training. Record
# peaks are rare, so predicting the raw level pulls hot days back toward normal.
# Changes ("+2,000 MW because tomorrow is 10F hotter") are much more common in
# the data, so the model can learn them, and adding yesterday back lets the
# forecast go above anything seen in training.
#
# Same features, same split and same tree-picking method as 05, so the two
# models can be compared fairly. 06_evaluate.R picks this one up automatically.
#
# Input:  ../data/processed/model_table.csv
# Output: ../results/predictions_xgboost_delta.csv, ../results/feature_importance_delta.csv,
#         ../results/models/xgb_delta_final.json
#
#   cd scripts
#   Rscript 05b_train_xgboost_delta.R

source("utils.R")
suppressPackageStartupMessages(library(xgboost))

mt <- read_csv(file.path(DATA_PROC, "model_table.csv"), show_col_types = FALSE) |>
  mutate(change = demand_mw - lag_24h)          # the new target

to_matrix <- function(d) as.matrix(d[, FEATURES])
dmat <- function(d) xgb.DMatrix(to_matrix(d), label = d$change)

train <- mt |> filter(split == "train")
val   <- mt |> filter(split == "val")
test  <- mt |> filter(split == "test")

params <- list(
  objective        = "reg:squarederror",
  eta              = 0.05,
  max_depth        = 6,
  min_child_weight = 5,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  eval_metric      = "rmse",
  seed             = 42
)
set.seed(42)
MAX_TREES <- 4000   # changes are learned more slowly, so allow more trees than in 05

# --- helpers that work with both older (1.x) and newer (2.x/3.x) xgboost versions ---
get_log <- function(m) {
  if (is.null(m)) return(NULL)
  l <- tryCatch(m$evaluation_log, error = function(e) NULL)
  if (is.null(l)) l <- attributes(m)$evaluation_log
  l
}
train_with_val <- function(dtrain, dval) {
  m <- tryCatch(
    xgb.train(params = params, data = dtrain, nrounds = MAX_TREES,
              evals = list(train = dtrain, val = dval), verbose = 0),
    error = function(e) NULL)
  if (is.null(get_log(m)) || nrow(get_log(m)) == 0) {
    m <- xgb.train(params = params, data = dtrain, nrounds = MAX_TREES,
                   watchlist = list(train = dtrain, val = dval), verbose = 0)
  }
  m
}

# ---- pick the number of trees on the validation year ----
# (RMSE of the change = RMSE of the forecast, since lag_24h is known exactly)
cat("Training on", nrow(train), "rows, validating on", nrow(val), "rows...\n")
m1  <- train_with_val(dmat(train), dmat(val))
log <- as_tibble(get_log(m1))
val_col    <- grep("val", names(log), value = TRUE)[1]
best_trees <- which.min(log[[val_col]])
cat(sprintf("Best number of trees: %d (val RMSE %.1f MW)\n", best_trees, min(log[[val_col]])))
if (best_trees > 0.95 * MAX_TREES) {
  cat("Note: best tree count is near the limit. Try raising MAX_TREES or eta.\n")
}

# ---- refit on train + val, predict test ----
trval <- bind_rows(train, val)
final <- xgb.train(params = params, data = dmat(trval), nrounds = best_trees, verbose = 0)
invisible(xgb.save(final, file.path(MODELS, "xgb_delta_final.json")))

pred_change <- predict(final, xgb.DMatrix(to_matrix(test)))
preds <- test |>
  transmute(time_utc, local_date, hour, month, demand_mw,
            model = "xgboost_delta",
            pred  = lag_24h + pred_change)
write_csv(preds, file.path(RESULTS, "predictions_xgboost_delta.csv"))

imp <- xgb.importance(feature_names = FEATURES, model = final) |> as_tibble()
write_csv(imp, file.path(RESULTS, "feature_importance_delta.csv"))

# ---- compare with the original model, overall and on the hottest days ----
cat(sprintf("\nTest set:  MAE %.0f MW | RMSE %.0f MW | MAPE %.2f%%\n",
            mae(preds$demand_mw, preds$pred), rmse(preds$demand_mw, preds$pred),
            mape(preds$demand_mw, preds$pred)))

orig_file <- file.path(RESULTS, "predictions_xgboost.csv")
if (file.exists(orig_file)) {
  both <- bind_rows(read_csv(orig_file, show_col_types = FALSE), preds)

  # "peak hours" = top 5% highest-demand hours of the test year
  cutoff <- quantile(test$demand_mw, 0.95)
  cat(sprintf("\nOriginal vs delta model (peak hours = demand above %.0f MW):\n", cutoff))
  both |>
    group_by(model) |>
    summarise(
      MAPE_all      = mape(demand_mw, pred),
      MAPE_peaks    = mape(demand_mw[demand_mw >= cutoff], pred[demand_mw >= cutoff]),
      bias_peaks_MW = mean(pred[demand_mw >= cutoff] - demand_mw[demand_mw >= cutoff]),
      .groups = "drop"
    ) |>
    print()
  cat("(bias < 0 means the model forecasts too LOW on peak hours)\n")
}
