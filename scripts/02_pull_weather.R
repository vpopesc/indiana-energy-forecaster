# 02_pull_weather.R
# Downloads hourly historical weather for Indiana cities inside MISO Zone 6 from
# Open-Meteo (free, no key), then builds one population-weighted "Indiana" series.
#
# Fort Wayne, South Bend and Muncie are left out on purpose: they are served by
# Indiana Michigan Power, which is in PJM, not MISO, so their load is not in Zone 6.
#
# Output: ../data/raw/weather_cities.csv   (one row per city per hour)
#         ../data/raw/weather_indiana.csv  (one weighted row per hour)
#
#   cd scripts
#   Rscript 02_pull_weather.R

source("utils.R")
suppressPackageStartupMessages(library(httr2))

# Approximate metro populations, used only as weights. Tweak if you like.
cities <- tribble(
  ~city,               ~lat,    ~lon,     ~pop,
  "Indianapolis",      39.7684, -86.1581, 2100000,
  "Gary/NW Indiana",   41.5934, -87.3464,  700000,  # NIPSCO territory
  "Evansville",        37.9716, -87.5711,  315000,
  "Lafayette",         40.4167, -86.8753,  230000,
  "Terre Haute",       39.4667, -87.4139,  185000,
  "Bloomington",       39.1653, -86.5264,  160000
)

end_date <- Sys.Date() - 2   # archive lags real time by a couple of days

fetch_city <- function(lat, lon) {
  res <- request("https://archive-api.open-meteo.com/v1/archive") |>
    req_url_query(
      latitude         = lat,
      longitude        = lon,
      start_date       = format(START_DATE),
      end_date         = format(end_date),
      hourly           = "temperature_2m,dew_point_2m,wind_speed_10m,cloud_cover",
      temperature_unit = "fahrenheit",
      wind_speed_unit  = "mph",
      timezone         = "GMT"
    ) |>
    req_retry(max_tries = 5) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  tibble(
    time_utc  = ymd_hm(res$hourly$time, tz = "UTC"),
    temp_f    = as.numeric(res$hourly$temperature_2m),
    dew_f     = as.numeric(res$hourly$dew_point_2m),
    wind_mph  = as.numeric(res$hourly$wind_speed_10m),
    cloud_pct = as.numeric(res$hourly$cloud_cover)
  )
}

weather_cities <- cities |>
  mutate(data = map2(lat, lon, \(la, lo) {
    cat("  downloading", la, lo, "\n")
    Sys.sleep(1)
    fetch_city(la, lo)
  })) |>
  unnest(data)

write_csv(weather_cities, file.path(DATA_RAW, "weather_cities.csv"))

# Population-weighted average across cities for each hour
weather_indiana <- weather_cities |>
  group_by(time_utc) |>
  summarise(across(c(temp_f, dew_f, wind_mph, cloud_pct),
                   \(x) weighted.mean(x, pop, na.rm = TRUE)),
            .groups = "drop") |>
  arrange(time_utc)

write_csv(weather_indiana, file.path(DATA_RAW, "weather_indiana.csv"))

cat(sprintf("\nSaved %d hourly rows (%s to %s)\n",
            nrow(weather_indiana), min(weather_indiana$time_utc), max(weather_indiana$time_utc)))
print(summary(weather_indiana$temp_f))
