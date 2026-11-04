# Tables 1 to 4 and A1: QCEW September to September estimates with placebo inference

source(here::here("code", "functions.R"))
source(here("code", "published.R"))

m = read.csv(here("data", "clean", "qcew_monthly.csv"), colClasses = c(state = "character"))
d = m[m$month == 9, ]
d$period = d$year

t1 = build_table(d, 2021, 2023, 2024, 1)
report(t1, compare(t1, pub$t1a, pub$t1b, pub$t2, "Tables 1 and 2"), "table1_2_qcew_1yr")

t3 = build_table(d, 2021, 2023, 2025, 2)
report(t3, compare(t3, pub$t3a, pub$t3b, pub$t4, "Tables 3 and 4"), "table3_4_qcew_2yr")

# Table A1: raw September fast-food counts
ff = d[d$type == "fastfood", ]
pool_rou = qualify(ff, setdiff(unique(d$state), c(ca, dropped)))
pool_ni = qualify(ff, setdiff(non_increasers, dropped))
groups = list(CA = ca, RoU = pool_rou, NI = pool_ni, Dube20 = setdiff(pool_ni, c("05", "54")))
counts = t(sapply(groups, function(s) sapply(2023:2024, function(y) sum(ff$emp[ff$state %in% s & ff$year == y]))))
ta1 = data.frame(group = names(groups), n_states = lengths(groups),
                 base_published = pub$ta1$base, base_replicated = counts[, 1],
                 post_published = pub$ta1$post, post_replicated = counts[, 2],
                 pct_change = 100 * (counts[, 2] / counts[, 1] - 1),
                 log_change = 100 * log(counts[, 2] / counts[, 1]))
ta1$dd_log = ta1$log_change[1] - ta1$log_change
ta1$dd_log[1] = NA
save_table(ta1, "tableA1_qcew_counts")
cat("\nTable A1\n")
print(ta1, digits = 6)
