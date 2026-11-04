# Why the two-year QCEW numbers differ: BLS revised the 2025 data after the authors downloaded it.
# This runs my code on the authors' own cleaned panel (shipped in their replication package).

source(here::here("code", "functions.R"))
source(here("code", "published.R"))

pkg = here("authors_package", "CEMN_Employment_2026")
if (!dir.exists(pkg)) {
  dir.create(here("authors_package"), showWarnings = FALSE)
  zip = here("authors_package", "CEMN_Employment_2026.zip")
  download.file("https://econweb.ucsd.edu/~j1clemens/replicationzips/CEMN_Employment_2026.zip", zip)
  unzip(zip, exdir = here("authors_package"))
}

a = read.csv(file.path(pkg, "data", "clean", "qcewclean.csv"), colClasses = c(state = "character"))
a = a[grepl("^[0-9]{2}$", a$state) & a$type %in% c("fastfood", "totalminus"), ]
a = data.frame(state = a$state, type = ifelse(a$type == "totalminus", "hwd", "fastfood"),
               year = a$year, month = a$month, emp = a$count)

# same code, authors' data: Tables 3 and 4
d = a[a$month == 9, ]
d$period = d$year
t3 = build_table(d, 2021, 2023, 2025, 2)
cmp = compare(t3, pub$t3a, pub$t3b, pub$t4, "Tables 3 and 4, authors' data")
save_table(cmp, "table3_4_qcew_2yr_authors_data")
cat("My code on the authors' data, Tables 3 and 4\n")
for (p in c("A", "B", "placebo")) {
  x = cmp[cmp$panel == p & cmp$column != "n_placebo", ]
  cat(sprintf("Panel %s: largest gap %.4f, %d of %d within 0.01\n", p, max(abs(x$difference)),
              sum(abs(x$difference) <= 0.01 + 1e-9), nrow(x)))
}

# where the two vintages differ
m = read.csv(here("data", "clean", "qcew_monthly.csv"), colClasses = c(state = "character"))
x = merge(a, m, by = c("state", "type", "year", "month"), suffixes = c("_authors", "_mine"))
x$changed = x$emp_authors != x$emp_mine
by_year = aggregate(changed ~ year + type, data = x, FUN = sum)
by_year$cells = aggregate(changed ~ year + type, data = x, FUN = length)$changed
save_table(by_year, "vintage_cells_changed")
cat("\nState by month cells that changed, by year\n")
print(reshape(by_year[, 1:3], idvar = "year", timevar = "type", direction = "wide"))
ca_sep = x[x$state == ca & x$month == 9 & x$year >= 2023, c("type", "year", "emp_authors", "emp_mine")]
print(ca_sep)
save_table(ca_sep, "vintage_california_september")
