# Downloads the raw public data. Run once. Needs curl, gunzip and awk (standard on Mac and Linux).
#   QCEW: BLS open data API, one file per quarter and industry. Only the statewide rows are kept.
#   QWI: Census LEHD release R2026Q2 (the release the paper uses), one file per state.
#        Only all-worker rows for the five industries are kept.
#   CPI-U: FRED series CPIAUCNS.

library(here)

# QCEW
codes = c("10", "44_45", "72", "722511", "722513")
for (yr in 2015:2025) for (q in 1:4) for (code in codes) {
  out = here("data", "raw", "qcew", sprintf("%d_q%d_%s.csv", yr, q, code))
  if (file.exists(out)) next
  tmp = tempfile(fileext = ".csv")
  download.file(sprintf("https://data.bls.gov/cew/data/api/%d/%d/industry/%s.csv", yr, q, code),
                tmp, quiet = TRUE)
  x = read.csv(tmp, colClasses = "character")
  write.csv(x[grepl("^[0-9]{2}000$", x$area_fips), ], out, row.names = FALSE)
  unlink(tmp)
}

# QWI
states = c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "dc", "fl", "ga", "hi", "id", "il",
           "in", "ia", "ks", "ky", "la", "me", "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne",
           "nv", "nh", "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri", "sc", "sd",
           "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", "wy")
base = "https://lehd.ces.census.gov/data/qwi/R2026Q2"
keep = "$8==\"0\" && $9==\"A00\" && $12==\"E0\" && $13==\"0\" && $14==\"0\" && $15>=2015"
for (level in c("ns", "n6")) {
  out = here("data", "raw", "qwi", paste0("qwi_states_", level, ".csv"))
  if (file.exists(out)) next
  inds = if (level == "ns") "($6==\"00\" || $6==\"44-45\" || $6==\"72\")" else "($6==\"722511\" || $6==\"722513\")"
  for (st in states) {
    url = sprintf("%s/%s/qwi_%s_se_f_gs_%s_op_u.csv.gz", base, st, st, level)
    awk = sprintf("awk -F, 'NR==1 || (%s && %s)'", inds, keep)
    rows = system(sprintf("curl -sf %s | gunzip -c | %s", url, awk), intern = TRUE)
    if (length(rows) < 2) {
      message("no QWI file for ", st)
      next
    }
    if (file.exists(out)) rows = rows[-1]
    cat(rows, file = out, sep = "\n", append = TRUE)
  }
}

# CPI-U, monthly, not seasonally adjusted
cpi = here("data", "raw", "cpi", "CPIAUCNS.csv")
if (!file.exists(cpi)) download.file("https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCNS", cpi, quiet = TRUE)

if (!file.exists(here("data", "raw", "download_log.txt"))) writeLines(c(paste("Downloaded on", Sys.Date()),
             "QCEW: https://data.bls.gov/cew/data/api/YEAR/QTR/industry/CODE.csv, statewide rows only",
             "QWI: https://lehd.ces.census.gov/data/qwi/R2026Q2/ST/qwi_ST_se_f_gs_LEVEL_op_u.csv.gz",
             "     rows with sex 0, agegrp A00, education E0, all firm ages and sizes, 2015 on",
             "CPI: https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCNS"),
           here("data", "raw", "download_log.txt"))
