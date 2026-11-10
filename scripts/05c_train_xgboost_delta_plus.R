# 05c_train_xgboost_delta_plus.R
# The change-based model from 05b, plus new features aimed at its biggest
# weakness: days when the weather flips (first day of a heat wave, a sudden
# cool-down, a cold snap). The model leans on "same hour yesterday", so it needs
# to be told clearly when tomorrow's weather is very different from today's.
#
# New features (all known by midnight, weather treated as a perfect forecast):
#   tmax_day, tmin_day      tomorrow's daily high and low
#   tmax_change             tomorrow's high minus today's high
#   tmean_change            tomorrow's average temperature minus today's
#   cool_deg_change         cooling degrees now vs same hour yesterday  (AC change)
#   heat_deg_change         heating degrees now vs same hour yesterday  (heating change)
#   tmax_3day, tmin_3day    hottest high / coldest low over the last 3 days
#   hot_streak              days in a row with a high of 85F or more
#   cold_streak             days in a row with a low of 20F or less
#
# The features are built here, not in 03, so models 05 and 05b stay unchanged
# and the comparison stays fair.
#
# Input:  ../data/processed/model_table.csv, ../data/raw/weather_indiana.csv
# Output: ../results/predictions_xgboost_delta_plus.csv,
#         ../results/feature_importance_delta_plus.csv,
#         ../results/models/xgb_delta_plus_final.json
#
#   cd scripts
#   Rscript 05c_train_xgboost_delta_plus.R

source("utils.R")
suppressPackageStartupMessages(library(xgboost))

mt      <- read_csv(file.path(DATA_PROC, "model_table.csv"), show_col_types = FALSE)
weather <- read_csv(file.path(DATA_RAW, "weather_indiana.csv"), show_col_types = FALSE)

# ---------------------------------------------------------------------------
# 1. Daily weather summaries (Indiana local days)
# ---------------------------------------------------------------------------
streak <- function(x) {                       # running count of TRUEs in a row
  out <- integer(length(x)); run <- 0L
  for (i in seq_along(x)) { run <- if (isTRUE(x[i])) run + 1L else 0L; out[i] <- run }
  out
}

daily <- weather |>
  mutate(local_date = as_date(with_tz(time_utc, LOCAL_TZ))) |>
  group_by(local_date) |>
  summarise(tmax_day  = max(temp_f, na.rm = TRUE),
            tmin_day  = min(temp_f, na.rm = TRUE),
            tmean_day = mean(temp_f, na.rm = TRUE),
            .groups = "drop") |>
  arrange(local_date) |>
  mutate(
    tmax_change  = tmax_day - lag(tmax_day),
    tmean_change = tmean_day - lag(tmean_day),
    tmax_3day    = pmax(tmax_day, lag(tmax_day), lag(tmax_day, 2)),
    tmin_3day    = pmin(tmin_day, lag(tmin_day), lag(tmin_day, 2)),
    hot_streak   = streak(tmax_day >= 85),
    cold_streak  = streak(tmin_day <= 20)
  ) |>
  select(-tmean_day)

# ---------------------------------------------------------------------------
# 2. Hourly change in heating / cooling need vs the same hour yesterday
# ---------------------------------------------------------------------------
hourly_deg <- weather |>
  transmute(time_utc,
            cool = pmax(temp_f - 65, 0),
            heat = pmax(65 - temp_f, 0))
yesterday_deg <- hourly_deg |>
  mutate(time_utc = time_utc + hours(24)) |>
  rename(cool_y = cool, heat_y = heat)

NEW_FEATURES <- c("tmax_day", "tmin_day", "tmax_change", "tmean_change",
                  "cool_deg_change", "heat_deg_change",
                  "tmax_3day", "tmin_3day", "hot_streak", "cold_streak")
ALL_FEATURES <- c(FEATURES, NEW_FEATURES)

mt <- mt |>
  left_join(daily, by = "local_date") |>
  left_join(yesterday_deg, by = "time_utc") |>
  mutate(cool_deg_change = cool_deg - cool_y,
         heat_deg_change = heat_deg - heat_y,
         change = demand_mw - lag_24h) |>
  drop_na(all_of(ALL_FEATURES))

# ---------------------------------------------------------------------------
# 3. Train exactly like 05b (same target, settings and tree-picking)
# ---------------------------------------------------------------------------
to_matrix <- function(d) as.matrix(d[, ALL_FEATURES])
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
MAX_TREES <- 4000

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

cat("Training on", nrow(train), "rows, validating on", nrow(val), "rows...\n")
m1  <- train_with_val(dmat(train), dmat(val))
log <- as_tibble(get_log(m1))
val_col    <- grep("val", names(log), value = TRUE)[1]
best_trees <- which.min(log[[val_col]])
cat(sprintf("Best number of trees: %d (val RMSE %.1f MW)\n", best_trees, min(log[[val_col]])))
if (best_trees > 0.95 * MAX_TREES) {
  cat("Note: best tree count is near the limit. Try raising MAX_TREES or eta.\n")
}

trval <- bind_rows(train, val)
final <- xgb.train(params = params, data = dmat(trval), nrounds = best_trees, verbose = 0)
invisible(xgb.save(final, file.path(MODELS, "xgb_delta_plus_final.json")))

preds <- test |>
  transmute(time_utc, local_date, hour, month, demand_mw,
            model = "xgboost_delta_plus",
            pred  = lag_24h + predict(final, xgb.DMatrix(to_matrix(test))))
write_csv(preds, file.path(RESULTS, "predictions_xgboost_delta_plus.csv"))

imp <- xgb.importance(feature_names = ALL_FEATURES, model = final) |> as_tibble()
write_csv(imp, file.path(RESULTS, "feature_importance_delta_plus.csv"))

# ---------------------------------------------------------------------------
# 4. Compare all three XGBoost versions on the SAME test hours
# ---------------------------------------------------------------------------
others <- c("xgboost", "xgboost_delta")
files  <- file.path(RESULTS, paste0("predictions_", others, ".csv"))
all_preds <- bind_rows(
  map(files[file.exists(files)], \(f) read_csv(f, show_col_types = FALSE)),
  preds
) |>
  semi_join(preds, by = "time_utc")           # same hours for every model

peak_cut  <- quantile(preds$demand_mw, 0.95)
flip_days <- test |> filter(abs(tmax_change) >= 10) |> distinct(local_date) |> pull()

cat(sprintf("\nComparison on the same %d test hours\n", nrow(preds)))
cat(sprintf("peaks = top 5%% of hours (above %.0f MW); flip days = high temp changed 10F+ from the day before (%d days)\n\n",
            peak_cut, length(flip_days)))
all_preds |>
  group_by(model) |>
  summarise(
    MAPE_all      = mape(demand_mw, pred),
    MAPE_peaks    = mape(demand_mw[demand_mw >= peak_cut], pred[demand_mw >= peak_cut]),
    bias_peaks_MW = mean(pred[demand_mw >= peak_cut] - demand_mw[demand_mw >= peak_cut]),
    MAPE_flipdays = mape(demand_mw[local_date %in% flip_days], pred[local_date %in% flip_days]),
    .groups = "drop"
  ) |>
  arrange(MAPE_all) |>
  print()

cat("\nTop 15 features (new ones marked *):\n")
imp |>
  head(15) |>
  mutate(Feature = ifelse(Feature %in% NEW_FEATURES, paste0(Feature, " *"), Feature)) |>
  select(Feature, Gain) |>
  print(n = 15)
