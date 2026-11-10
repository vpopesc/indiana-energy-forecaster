# Lab notes

Running log of decisions, problems and results. Newest entries at the bottom.

---

## Setup decisions

**Target region: MISO Local Resource Zone 6 (LRZ 6).**
Indiana is split between two grid operators. Most of the state is in MISO; the
northeast (Indiana Michigan Power: Fort Wayne, South Bend, Muncie) is in PJM.
EIA publishes hourly demand for MISO's zones, and Zone 6 is the Indiana one.

**Known limitation: Zone 6 includes a small piece of western Kentucky**
(Big Rivers Electric). EIA only publishes the zone as one number, so the Kentucky
part can't be removed from the hourly data. It is a small share of the zone's load.
To keep the model Indiana-focused, the weather only uses Indiana cities.
(An Indiana-only alternative would be summing the utilities' FERC Form 714
hourly loads, but those are only published once a year, so the data lags by a year.)

**Weather cities:** Indianapolis, Gary/NW Indiana, Evansville, Lafayette,
Terre Haute, Bloomington, weighted by rough metro population. Fort Wayne and
South Bend are left out because they are in PJM.

**Time zones:** everything is stored in UTC. EIA's API returns UTC when
`frequency=hourly`. (MISO's own files use EST all year, which is a trap if you
ever pull from MISO directly.) Calendar features use Indiana local time.

**Forecast setup:** day-ahead. At midnight, forecast all 24 hours of the next
day. Features may only use demand up to the end of the previous day.

**Weather = perfect forecast.** The model is trained and tested on *actual*
weather, standing in for the weather forecast you'd have at midnight. Real
forecasts have errors, so real-world accuracy would be a bit worse than the
test numbers. Worth saying in the README.

**Split:** last 365 days = test, the 365 before = validation (used to choose
the number of trees), everything earlier = train.

---

## Log

### YYYY-MM-DD
- Pulled data: ___ rows from ___ to ___
- Missing hours / anomalies flagged: ___
- Does yearly average demand trend up (data centers)? ___
- Results: naive_week MAPE ___ %, linear ___ %, xgboost ___ %
- Worst days and why: ___
- Next idea to try: ___
