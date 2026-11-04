# Tables 5 to 8, A2, A3 and A4: QWI estimates with placebo inference

source(here::here("code", "functions.R"))
source(here("code", "published.R"))

q = read.csv(here("data", "clean", "qwi_quarterly.csv"), colClasses = c(state = "character"))
q$period = q$year * 10 + q$qtr

t5 = build_table(q, 20213, 20233, 20243, 1)
report(t5, compare(t5, pub$t5a, pub$t5b, pub$t6, "Tables 5 and 6"), "table5_6_qwi_1yr")

t7 = build_table(q, 20213, 20233, 20253, 2)
report(t7, compare(t7, pub$t7a, pub$t7b, pub$t8, "Tables 7 and 8"), "table7_8_qwi_2yr")

ta3 = build_table(q, 20214, 20234, 20244, 1)
report(ta3, compare(ta3, pub$ta3a, pub$ta3b, pub$ta4, "Tables A3 and A4"), "tableA3_A4_qwi_q4base")

# Table A2: raw Q3 fast-food counts
ff = q[q$type == "fastfood", ]
pool_rou = qualify(ff, setdiff(unique(q$state), c(ca, dropped)))
pool_ni = qualify(ff, setdiff(non_increasers, dropped))
groups = list(CA = ca, RoU = pool_rou, NI = pool_ni, Dube20 = setdiff(pool_ni, c("05", "54")))
counts = t(sapply(groups, function(s) sapply(c(20233, 20243), function(p) sum(ff$emp[ff$state %in% s & ff$period == p]))))
ta2 = data.frame(group = names(groups), n_states = lengths(groups),
                 base_published = pub$ta2$base, base_replicated = counts[, 1],
                 post_published = pub$ta2$post, post_replicated = counts[, 2],
                 pct_change = 100 * (counts[, 2] / counts[, 1] - 1),
                 log_change = 100 * log(counts[, 2] / counts[, 1]))
ta2$dd_log = ta2$log_change[1] - ta2$log_change
ta2$dd_log[1] = NA
save_table(ta2, "tableA2_qwi_counts")
cat("\nTable A2\n")
print(ta2, digits = 6)
