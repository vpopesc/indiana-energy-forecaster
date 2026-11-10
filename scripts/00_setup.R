# 00_setup.R
# Run once. Installs the R packages the project needs (skips ones you already
# have) and checks that your EIA API key is visible to R.
#
#   cd scripts
#   Rscript 00_setup.R

pkgs <- c("tidyverse", "lubridate", "httr2", "jsonlite", "xgboost")

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  cat("All packages already installed.\n")
}

for (p in pkgs) cat(sprintf("  %-10s %s\n", p, as.character(packageVersion(p))))

key <- Sys.getenv("EIA_API_KEY")
if (key == "") {
  cat("\nEIA_API_KEY is NOT set. Add this line to ~/.bashrc, then open a new terminal:\n",
      "  export EIA_API_KEY=\"your_key_here\"\n")
} else {
  cat("\nEIA_API_KEY found (ends in ...", substr(key, nchar(key) - 3, nchar(key)), ")\n", sep = "")
}
