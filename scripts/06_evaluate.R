# 06_evaluate.R
# Compares every model on the test year and makes the charts for the README.
#
# Input:  ../results/predictions_*.csv, ../results/feature_importance.csv
# Output: ../results/metrics.csv, ../results/worst_days.csv, ../results/figures/*.png
#
#   cd scripts
#   Rscript 06_evaluate.R

source("utils.R")

preds <- list.files(RESULTS, pattern = "^predictions_.*\\.csv$", full.names = TRUE) |>
  map(\(f) read_csv(f, show_col_types = FALSE)) |>
  bind_rows() |>
  mutate(local_time = with_tz(time_utc, LOCAL_TZ),
         abs_pct_err = 100 * abs(demand_mw - pred) / demand_mw)

# ---- 1. overall metrics ----
metrics <- preds |>
  group_by(model) |>
  summarise(MAE_MW = mae(demand_mw, pred), RMSE_MW = rmse(demand_mw, pred),
            MAPE_pct = mape(demand_mw, pred), .groups = "drop") |>
  arrange(MAPE_pct)
write_csv(metrics, file.path(RESULTS, "metrics.csv"))
cat("Test-year results:\n"); print(metrics)

theme_set(theme_minimal(base_size = 12))
save_fig <- function(p, name, w = 9, h = 5) {
  ggsave(file.path(FIGS, name), p, width = w, height = h, dpi = 150, bg = "white")
  cat("  saved", name, "\n")
}

# ---- 2. error by hour of day ----
p_hour <- preds |>
  group_by(model, hour) |>
  summarise(MAPE = mean(abs_pct_err, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(hour, MAPE, colour = model)) +
  geom_line(linewidth = 1) +
  labs(title = "Forecast error by hour of day (test year)",
       x = "Hour (Indiana time)", y = "MAPE (%)", colour = NULL)
save_fig(p_hour, "error_by_hour.png")

# ---- 3. error by month ----
p_month <- preds |>
  group_by(model, month) |>
  summarise(MAPE = mean(abs_pct_err, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(factor(month, labels = month.abb), MAPE, colour = model, group = model)) +
  geom_line(linewidth = 1) + geom_point() +
  labs(title = "Forecast error by month (test year)", x = NULL, y = "MAPE (%)", colour = NULL)
save_fig(p_month, "error_by_month.png")

# ---- 4. actual vs forecast for the peak-demand week ----
best_model <- metrics$model[1]
peak_day <- preds |> filter(model == best_model) |> slice_max(demand_mw, n = 1) |> pull(local_date)
week_models <- unique(c("naive_week", "xgboost", "xgboost_delta", "xgboost_delta_plus", best_model))
week <- preds |>
  filter(local_date >= peak_day - 3, local_date <= peak_day + 3,
         model %in% week_models)
p_week <- ggplot(week, aes(local_time)) +
  geom_line(aes(y = demand_mw), data = distinct(week, local_time, demand_mw),
            colour = "black", linewidth = 1) +
  geom_line(aes(y = pred, colour = model), linewidth = 0.8, linetype = "dashed") +
  labs(title = paste("Peak week around", peak_day, "- black = actual"),
       x = NULL, y = "Demand (MW)", colour = "Forecast")
save_fig(p_week, "peak_week.png", w = 10)

# ---- 5. error vs temperature: does the model miss in the heat? ----
temps <- read_csv(file.path(DATA_PROC, "model_table.csv"), show_col_types = FALSE) |>
  select(time_utc, temp_f)
p_temp <- preds |>
  filter(str_detect(model, "xgboost|linear")) |>
  left_join(temps, by = "time_utc") |>
  mutate(temp_bin = 5 * floor(temp_f / 5)) |>
  group_by(model, temp_bin) |>
  summarise(bias_pct = 100 * mean((pred - demand_mw) / demand_mw), n = n(), .groups = "drop") |>
  filter(n >= 20) |>
  ggplot(aes(temp_bin, bias_pct, colour = model)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(linewidth = 1) + geom_point() +
  labs(title = "Forecast bias by temperature (test year)",
       subtitle = "Below 0 = forecast too low",
       x = "Temperature (F, 5-degree bins)", y = "Average error (%)", colour = NULL)
save_fig(p_temp, "bias_by_temperature.png")

# ---- 6. feature importance ----
if (file.exists(file.path(RESULTS, "feature_importance.csv"))) {
  imp <- read_csv(file.path(RESULTS, "feature_importance.csv"), show_col_types = FALSE)
  p_imp <- imp |> slice_max(Gain, n = 15) |>
    ggplot(aes(Gain, fct_reorder(Feature, Gain))) +
    geom_col(fill = "steelblue") +
    labs(title = "XGBoost feature importance (gain)", x = "Gain", y = NULL)
  save_fig(p_imp, "feature_importance.png", w = 7)
}

# ---- 7. worst days for the best model ----
worst <- preds |>
  filter(model == best_model) |>
  group_by(local_date) |>
  summarise(MAPE = mean(abs_pct_err), peak_mw = max(demand_mw),
            avg_actual = round(mean(demand_mw)), avg_forecast = round(mean(pred)),
            hours = n(), .groups = "drop") |>
  slice_max(MAPE, n = 10)
write_csv(worst, file.path(RESULTS, "worst_days.csv"))
cat("\nWorst 10 days for", best_model, "(look these up: storms? holidays? cold snaps?)\n")
cat("hours = how many hours of that day are in the test set (24 = full day)\n")
print(worst)
