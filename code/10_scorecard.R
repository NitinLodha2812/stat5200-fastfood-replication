# Scorecard: how many published numbers my code reproduces

source(here::here("code", "functions.R"))

rt = function(f) read.csv(here("results", "tables", paste0(f, ".csv")))

# a number matches when it rounds to the published value
tol = function(x) ifelse(x$column == "estimate", 0.0005, ifelse(x$column == "p_value", 0.00005, 0.005)) + 1e-9
score = function(f, label) {
  x = rt(f)
  x = x[x$column != "n_placebo" & x$column != "base", ]
  data.frame(table = label, numbers = nrow(x), match = sum(abs(x$difference) <= tol(x)),
             largest_gap = round(max(abs(x$difference)), 3))
}
# job counts must match exactly
counts = function(f, label) {
  x = rt(f)
  d = c(x$base_replicated - x$base_published, x$post_replicated - x$post_published)
  data.frame(table = label, numbers = length(d), match = sum(d == 0), largest_gap = max(abs(d)))
}

sc = rbind(score("table1_2_qcew_1yr", "Tables 1, 2: QCEW, 1 year"),
           score("table3_4_qcew_2yr", "Tables 3, 4: QCEW, 2 years, current data"),
           score("table3_4_qcew_2yr_authors_data", "Tables 3, 4: QCEW, 2 years, authors' data"),
           score("table5_6_qwi_1yr", "Tables 5, 6: QWI, 1 year"),
           score("table7_8_qwi_2yr", "Tables 7, 8: QWI, 2 years"),
           score("tableA3_A4_qwi_q4base", "Tables A3, A4: QWI, Q4 base"),
           counts("tableA1_qcew_counts", "Table A1: QCEW job counts"),
           counts("tableA2_qwi_counts", "Table A2: QWI job counts"))
save_table(sc, "scorecard")
print(sc)
