# utils.R
# Shared settings and helper functions. Every other script runs source("utils.R")
# at the top, so anything you change here (paths, dates, split) changes everywhere.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# ---- paths (all relative to scripts/, which is where you run things from) ----
DATA_RAW  <- "../data/raw"
DATA_PROC <- "../data/processed"
RESULTS   <- "../results"
FIGS      <- "../results/figures"
MODELS    <- "../results/models"

# ---- project settings ----
LOCAL_TZ   <- "America/Indiana/Indianapolis"  # Indiana local time (observes DST)
START_DATE <- as.Date("2019-01-01")           # first day of data we pull
TEST_DAYS  <- 365                             # last year of data = test set
VAL_DAYS   <- 365                             # year before that = validation set

# Stop early with a clear message if the script is run from the wrong folder.
check_wd <- function() {
  if (!dir.exists("../data") || !file.exists("utils.R")) {
    stop("Run scripts from inside the scripts/ folder, e.g.\n  cd scripts\n  Rscript 01_pull_demand.R",
         call. = FALSE)
  }
}
check_wd()

# ---- error metrics ----
mae  <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mape <- function(actual, pred) 100 * mean(abs(actual - pred) / actual, na.rm = TRUE)

# ---- train / validation / test split by local date ----
# Time series must be split by time, never randomly: the model trains on the
# past and is tested on a future it has never seen.
add_split <- function(df) {
  last_day   <- max(df$local_date)
  test_start <- last_day - TEST_DAYS + 1
  val_start  <- test_start - VAL_DAYS
  df |>
    mutate(split = case_when(
      local_date >= test_start ~ "test",
      local_date >= val_start  ~ "val",
      TRUE                     ~ "train"
    ))
}

# ---- the features the models are allowed to use ----
# Everything here is known by midnight before the day being forecast
# (weather is treated as a perfect forecast, see lab_notes).
FEATURES <- c(
  # calendar
  "hour", "dow", "month", "doy", "is_weekend", "is_holiday", "xmas_week", "trend_years",
  # weather (population-weighted Indiana average)
  "temp_f", "dew_f", "wind_mph", "cloud_pct", "heat_deg", "cool_deg",
  "temp_roll24", "temp_change24",
  # past demand
  "lag_24h", "lag_48h", "lag_168h", "prev_day_mean", "prev_day_max", "prev_day_last"
)
