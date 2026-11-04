# Runs the whole replication from the raw data. Open R in this folder and run
#   Rscript code/00_run_all.R
# 01_download_data.R skips files that are already in data/raw

library(here)

scripts = c("01_download_data.R", "02_clean_qcew.R", "03_clean_qwi.R", "04_qcew_tables.R", "05_qwi_tables.R",
            "06_vintage_check.R", "07_figures.R", "08_medians_elasticities.R", "09_extensions.R",
            "10_scorecard.R")
log = here("results", "run_log.txt")
sink(log, split = TRUE)
for (s in scripts) {
  cat("\n==========", s, "==========\n")
  source(here("code", s), local = new.env(), echo = FALSE)
}
sink()
writeLines(capture.output(sessionInfo()), here("results", "session_info.txt"))
