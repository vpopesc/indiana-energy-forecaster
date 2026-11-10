# 01_pull_demand.R
# Downloads hourly electricity demand for MISO Local Resource Zone 6
# (Indiana, plus a small slice of western Kentucky) from the EIA API.
#
# Source: EIA-930 "region-sub-ba-data" (demand by subregion of a balancing area).
# Output: ../data/raw/demand_zone6.csv  with columns time_utc, demand_mw
#
#   cd scripts
#   Rscript 01_pull_demand.R

source("utils.R")
suppressPackageStartupMessages(library(httr2))

key <- Sys.getenv("EIA_API_KEY")
if (key == "") stop("EIA_API_KEY is not set. See 00_setup.R.", call. = FALSE)

BASE   <- "https://api.eia.gov/v2/electricity/rto/region-sub-ba-data"
PARENT <- "MISO"
ZONE   <- "0006"   # MISO subregion code for Local Resource Zone 6

# ---- 1. sanity check: list MISO subregions so you can see what "0006" is ----
facets <- request(paste0(BASE, "/facet/subba")) |>
  req_url_query(api_key = key) |>
  req_retry(max_tries = 3) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE)

subbas <- as_tibble(facets$response$facets)
cat("Subregion codes that look like MISO zones:\n")
print(subbas |> filter(str_detect(name, regex("zone", ignore_case = TRUE))))

# ---- 2. download in pages (the API returns at most 5000 rows per request) ----
fetch_page <- function(offset) {
  request(paste0(BASE, "/data/")) |>
    req_url_query(
      api_key               = key,
      frequency             = "hourly",          # "hourly" = timestamps in UTC
      `data[0]`             = "value",
      `facets[parent][]`    = PARENT,
      `facets[subba][]`     = ZONE,
      start                 = format(START_DATE, "%Y-%m-%dT00"),
      end                   = format(Sys.Date(), "%Y-%m-%dT00"),
      `sort[0][column]`     = "period",
      `sort[0][direction]`  = "asc",
      offset                = offset,
      length                = 5000
    ) |>
    req_retry(max_tries = 5) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
}

pages  <- list()
offset <- 0
repeat {
  res   <- fetch_page(offset)
  chunk <- as_tibble(res$response$data)
  if (nrow(chunk) == 0) break
  pages[[length(pages) + 1]] <- chunk
  cat(sprintf("  got rows %d - %d of %s\n", offset + 1, offset + nrow(chunk), res$response$total))
  offset <- offset + nrow(chunk)
  if (nrow(chunk) < 5000) break
  Sys.sleep(0.5)  # be polite to the API
}

if (length(pages) == 0) {
  stop("No rows came back. Check the subregion list printed above and update ZONE.", call. = FALSE)
}

raw <- bind_rows(pages)
cat("\nSubregion name in the data:", unique(raw$`subba-name`), "\n")
cat("Units:", unique(raw$`value-units`), "\n")

# ---- 3. tidy and save ----
demand <- raw |>
  transmute(
    time_utc  = ymd_h(period, tz = "UTC"),
    demand_mw = as.numeric(value)          # MWh in one hour = average MW
  ) |>
  distinct(time_utc, .keep_all = TRUE) |>
  arrange(time_utc)

write_csv(demand, file.path(DATA_RAW, "demand_zone6.csv"))

cat(sprintf("\nSaved %d hourly rows (%s to %s) -> %s\n",
            nrow(demand), min(demand$time_utc), max(demand$time_utc),
            file.path(DATA_RAW, "demand_zone6.csv")))
print(summary(demand$demand_mw))
