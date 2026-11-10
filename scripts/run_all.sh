#!/usr/bin/env bash
# Runs the whole pipeline in order. From inside scripts/:
#   bash run_all.sh
# Stops at the first script that fails.
set -e

for s in 01_pull_demand.R 02_pull_weather.R 03_build_features.R \
         04_baselines.R 05_train_xgboost.R 05b_train_xgboost_delta.R \
         05c_train_xgboost_delta_plus.R          06_evaluate.R; do
  echo
  echo "==================== $s ===================="
  Rscript "$s"
done

echo
echo "Done. Results are in ../results, charts in ../results/figures"
