# 05_train_xgboost.R
# Trains the main model: gradient-boosted trees (XGBoost).
#
# How the number of trees is chosen:
#   1. Train on "train", watch the error on "val" after every tree.
#   2. Pick the tree count where val error was lowest (more trees after that = overfitting).
#   3. Refit on train + val with that many trees, then predict the untouched "test" year.
#
# Input:  ../data/processed/model_table.csv
# Output: ../results/predictions_xgboost.csv, ../results/feature_importance.csv,
#         ../results/models/xgb_final.json
#
#   cd scripts
#   Rscript 05_train_xgboost.R

source("utils.R")
suppressPackageStartupMessages(library(xgboost))

mt <- read_csv(file.path(DATA_PROC, "model_table.csv"), show_col_types = FALSE)

to_matrix <- function(d) as.matrix(d[, FEATURES])
dmat <- function(d) xgb.DMatrix(to_matrix(d), label = d$demand_mw)

train <- mt |> filter(split == "train")
val   <- mt |> filter(split == "val")
test  <- mt |> filter(split == "test")

params <- list(
  objective        = "reg:squarederror",
  eta              = 0.05,   # learning rate: small steps, many trees
  max_depth        = 6,      # how complex each tree can be
  min_child_weight = 5,
  subsample        = 0.8,    # each tree sees 80% of rows ...
  colsample_bytree = 0.8,    # ... and 80% of features (reduces overfitting)
  eval_metric      = "rmse",
  seed             = 42      # same results every run
)
set.seed(42)
MAX_TREES <- 2000

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
  if (is.null(get_log(m)) || nrow(get_log(m)) == 0) {      # older xgboost uses "watchlist"
    m <- xgb.train(params = params, data = dtrain, nrounds = MAX_TREES,
                   watchlist = list(train = dtrain, val = dval), verbose = 0)
  }
  m
}

# ---- step 1-2: find the best number of trees ----
cat("Training on", nrow(train), "rows, validating on", nrow(val), "rows...\n")
m1  <- train_with_val(dmat(train), dmat(val))
log <- as_tibble(get_log(m1))
val_col    <- grep("val", names(log), value = TRUE)[1]
best_trees <- which.min(log[[val_col]])
cat(sprintf("Best number of trees: %d (val RMSE %.1f MW)\n", best_trees, min(log[[val_col]])))
write_csv(log, file.path(RESULTS, "xgb_training_log.csv"))

# ---- step 3: refit on train + val, predict test ----
trval <- bind_rows(train, val)
final <- xgb.train(params = params, data = dmat(trval), nrounds = best_trees, verbose = 0)
invisible(xgb.save(final, file.path(MODELS, "xgb_final.json")))

preds <- test |>
  transmute(time_utc, local_date, hour, month, demand_mw,
            model = "xgboost",
            pred  = predict(final, xgb.DMatrix(to_matrix(test))))
write_csv(preds, file.path(RESULTS, "predictions_xgboost.csv"))

imp <- xgb.importance(feature_names = FEATURES, model = final) |> as_tibble()
write_csv(imp, file.path(RESULTS, "feature_importance.csv"))

cat(sprintf("\nTest set (%s to %s):  MAE %.0f MW | RMSE %.0f MW | MAPE %.2f%%\n",
            min(test$local_date), max(test$local_date),
            mae(preds$demand_mw, preds$pred), rmse(preds$demand_mw, preds$pred),
            mape(preds$demand_mw, preds$pred)))
cat("\nTop 10 features by gain:\n")
print(imp |> select(Feature, Gain) |> head(10))
