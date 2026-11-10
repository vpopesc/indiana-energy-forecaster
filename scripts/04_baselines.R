# 04_baselines.R
# Simple forecasts the real model has to beat. If XGBoost can't beat
# "same hour last week", something is wrong.
#
#   naive_week : demand at the same hour 7 days earlier
#   naive_day  : demand at the same hour 1 day earlier
#   linear     : linear regression on hour-of-day, weekend, temperature and yesterday
#
# Input:  ../data/processed/model_table.csv
# Output: ../results/predictions_baselines.csv  (test set only, long format)
#
#   cd scripts
#   Rscript 04_baselines.R

source("utils.R")

mt <- read_csv(file.path(DATA_PROC, "model_table.csv"), show_col_types = FALSE)

train <- mt |> filter(split %in% c("train", "val"))   # baselines don't need tuning, so use both
test  <- mt |> filter(split == "test")

# Linear regression: hourly shape that differs on weekends, U-shaped temperature
# effect (heating and cooling), plus yesterday's demand at the same hour.
lin <- lm(demand_mw ~ factor(hour) * is_weekend + is_holiday +
            heat_deg + cool_deg + I(cool_deg^2) + dew_f + lag_24h + lag_168h,
          data = train)
cat(sprintf("Linear model R^2 on train: %.3f\n\n", summary(lin)$r.squared))

preds <- test |>
  transmute(time_utc, local_date, hour, month, demand_mw,
            naive_week = lag_168h,
            naive_day  = lag_24h,
            linear     = predict(lin, newdata = test)) |>
  pivot_longer(c(naive_week, naive_day, linear), names_to = "model", values_to = "pred")

write_csv(preds, file.path(RESULTS, "predictions_baselines.csv"))

preds |>
  group_by(model) |>
  summarise(MAE_MW = mae(demand_mw, pred), RMSE_MW = rmse(demand_mw, pred),
            MAPE_pct = mape(demand_mw, pred)) |>
  arrange(MAPE_pct) |>
  print()
