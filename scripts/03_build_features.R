# 03_build_features.R
# Cleans the demand data, joins it with weather, and builds every feature the
# models use. This is the most important script in the project: if a feature
# here "peeks" at information you would not have at forecast time, every result
# downstream is fake.
#
# The forecasting setup: at midnight (Indiana time) we forecast all 24 hours of
# the next day. So for a target hour on day D we may use:
#   - all demand up to the end of day D-1   (lag_24h, lag_48h, lag_168h, prev_day_*)
#   - weather for day D                     (treated as a perfect forecast)
#   - the calendar
#
# Input:  ../data/raw/demand_zone6.csv, ../data/raw/weather_indiana.csv
# Output: ../data/processed/model_table.csv
#
#   cd scripts
#   Rscript 03_build_features.R

source("utils.R")

demand  <- read_csv(file.path(DATA_RAW, "demand_zone6.csv"),   show_col_types = FALSE)
weather <- read_csv(file.path(DATA_RAW, "weather_indiana.csv"), show_col_types = FALSE)

# ---------------------------------------------------------------------------
# 1. Clean demand
# ---------------------------------------------------------------------------

# Make sure every hour exists (missing hours become NA rows)
all_hours <- tibble(time_utc = seq(min(demand$time_utc), max(demand$time_utc), by = "hour"))
df <- all_hours |> left_join(demand, by = "time_utc")
cat("Missing hours in raw data:", sum(is.na(df$demand_mw)), "\n")

# Flag obviously bad values: zeros/negatives, absurd levels, and one-hour spikes
med <- median(df$demand_mw, na.rm = TRUE)
prev_h <- lag(df$demand_mw)
next_h <- lead(df$demand_mw)
spike <- !is.na(prev_h) & !is.na(next_h) &
  abs(df$demand_mw / prev_h - 1) > 0.25 &
  abs(df$demand_mw / next_h - 1) > 0.25
bad <- !is.na(df$demand_mw) &
  (df$demand_mw <= 0 | df$demand_mw < 0.3 * med | df$demand_mw > 2.5 * med | spike)
cat("Values flagged as anomalies:", sum(bad), "\n")
df$demand_mw[bad] <- NA

# "Stuck" values: the exact same number 4+ hours in a row is a reporting glitch
r <- rle(round(df$demand_mw, 1))
stuck_runs <- which(!is.na(r$values) & r$lengths >= 4)
ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
stuck <- unlist(map(stuck_runs, \(i) starts[i]:ends[i]))
cat("Hours flagged as stuck (same value 4+ hours):", length(stuck), "\n")
df$demand_mw[stuck] <- NA

# "Flat" days: a real day always swings a lot between night and afternoon.
# If a day's max-min swing is less than 20% of the typical swing for that month,
# the source almost certainly filled it with placeholder numbers, so drop the day.
# Kept strict on purpose: mild weekends and cold days can be genuinely flat-ish.
day_check <- df |>
  mutate(d = as_date(with_tz(time_utc, LOCAL_TZ))) |>
  group_by(d) |>
  filter(sum(!is.na(demand_mw)) >= 12) |>
  summarise(range_pct = (max(demand_mw, na.rm = TRUE) - min(demand_mw, na.rm = TRUE)) /
                          mean(demand_mw, na.rm = TRUE),
            .groups = "drop") |>
  group_by(m = month(d)) |>
  mutate(typical = median(range_pct)) |>
  ungroup()
flat_days <- day_check |> filter(range_pct < 0.20 * typical) |> pull(d)
cat("Flat days removed:", length(flat_days), "\n")
if (length(flat_days) > 0) print(flat_days)
df$demand_mw[as_date(with_tz(df$time_utc, LOCAL_TZ)) %in% flat_days] <- NA

# Fill short gaps (<= 3 hours) by straight-line interpolation; leave long gaps as NA
fill_short_gaps <- function(x, max_gap = 3) {
  r <- rle(is.na(x))
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  filled <- approx(seq_along(x), x, xout = seq_along(x), rule = 1)$y
  out <- x
  for (i in which(r$values & r$lengths <= max_gap)) {
    out[starts[i]:ends[i]] <- filled[starts[i]:ends[i]]
  }
  out
}
df$demand_mw <- fill_short_gaps(df$demand_mw)
cat("Hours still missing after filling short gaps:", sum(is.na(df$demand_mw)), "\n")

# ---------------------------------------------------------------------------
# 2. Calendar features (in Indiana local time)
# ---------------------------------------------------------------------------

us_holidays <- function(years) {
  nth_wday <- function(y, m, wd, n) {               # wd: 1 = Monday ... 7 = Sunday
    d <- seq(make_date(y, m, 1), by = "day", length.out = days_in_month(make_date(y, m, 1)))
    d[wday(d, week_start = 1) == wd][n]
  }
  last_wday <- function(y, m, wd) {
    d <- seq(make_date(y, m, 1), by = "day", length.out = days_in_month(make_date(y, m, 1)))
    tail(d[wday(d, week_start = 1) == wd], 1)
  }
  observed <- function(d) {                          # Sat -> Fri, Sun -> Mon
    case_when(wday(d, week_start = 1) == 6 ~ d - 1,
              wday(d, week_start = 1) == 7 ~ d + 1,
              TRUE ~ d)
  }
  map(years, function(y) {
    fixed <- c(make_date(y, 1, 1), make_date(y, 7, 4), make_date(y, 12, 25))
    thanksgiving <- nth_wday(y, 11, 4, 4)
    c(fixed, observed(fixed),
      last_wday(y, 5, 1),          # Memorial Day
      nth_wday(y, 9, 1, 1),        # Labor Day
      thanksgiving, thanksgiving + 1,
      make_date(y, 12, 24), make_date(y, 12, 31))
  }) |> reduce(c) |> unique()
}

df <- df |>
  mutate(
    local_time  = with_tz(time_utc, LOCAL_TZ),
    local_date  = as_date(local_time),
    hour        = hour(local_time),
    dow         = wday(local_time, week_start = 1),    # 1 = Monday
    month       = month(local_time),
    doy         = yday(local_time),
    is_weekend  = as.integer(dow >= 6),
    trend_years = as.numeric(difftime(time_utc, min(time_utc), units = "days")) / 365.25
  )

holidays <- us_holidays(unique(year(df$local_date)))
df <- df |>
  mutate(
    is_holiday = as.integer(local_date %in% holidays),
    xmas_week  = as.integer((month == 12 & day(local_date) >= 24) | (month == 1 & day(local_date) == 1))
  )

# ---------------------------------------------------------------------------
# 3. Weather features
# ---------------------------------------------------------------------------

roll_mean <- function(x, n) as.numeric(stats::filter(x, rep(1 / n, n), sides = 1))

df <- df |>
  left_join(weather, by = "time_utc") |>
  arrange(time_utc) |>
  mutate(
    heat_deg      = pmax(65 - temp_f, 0),    # how far below 65F -> heating demand
    cool_deg      = pmax(temp_f - 65, 0),    # how far above 65F -> AC demand
    temp_roll24   = roll_mean(temp_f, 24),   # buildings "remember" the last day's heat/cold
    temp_change24 = temp_f - lag(temp_f, 24)
  )

# ---------------------------------------------------------------------------
# 4. Demand lags (only information known by midnight before day D)
# ---------------------------------------------------------------------------

shifted <- function(hrs, name) {
  df |> select(time_utc, demand_mw) |>
    mutate(time_utc = time_utc + hours(hrs)) |>
    rename(!!name := demand_mw)
}

df <- df |>
  left_join(shifted(24,  "lag_24h"),  by = "time_utc") |>
  left_join(shifted(48,  "lag_48h"),  by = "time_utc") |>
  left_join(shifted(168, "lag_168h"), by = "time_utc")

safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

prev_day <- df |>
  arrange(time_utc) |>
  group_by(local_date) |>
  summarise(
    prev_day_mean = mean(demand_mw, na.rm = TRUE),
    prev_day_max  = safe_max(demand_mw),
    prev_day_last = last(demand_mw),           # demand at 11pm, the last hour we know
    .groups = "drop"
  ) |>
  mutate(local_date = local_date + 1,          # yesterday's stats become today's features
         prev_day_mean = ifelse(is.nan(prev_day_mean), NA, prev_day_mean))

df <- df |> left_join(prev_day, by = "local_date")

# ---------------------------------------------------------------------------
# 5. Final table
# ---------------------------------------------------------------------------

model_table <- df |>
  select(time_utc, local_date, demand_mw, all_of(FEATURES)) |>
  filter(!is.na(demand_mw)) |>
  drop_na(all_of(FEATURES)) |>
  add_split()

write_csv(model_table, file.path(DATA_PROC, "model_table.csv"))

cat(sprintf("\nSaved %d rows x %d columns -> %s\n",
            nrow(model_table), ncol(model_table), file.path(DATA_PROC, "model_table.csv")))
print(model_table |>
        group_by(split) |>
        summarise(rows = n(), first_day = min(local_date), last_day = max(local_date)) |>
        arrange(first_day))
