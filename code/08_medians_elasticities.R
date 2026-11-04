# Section 4.1 and 4.2 numbers: median of the 8 estimates, implied jobs lost, wage effect and elasticity

source(here::here("code", "functions.R"))

read_t = function(f) read.csv(here("results", "tables", paste0(f, ".csv")))
est = function(f) {
  x = read_t(f)
  x = x[x$panel == "placebo" & x$column == "estimate", ]
  c(published = median(x$published), replicated = median(x$replicated))
}
med = rbind(qcew_1yr = est("table1_2_qcew_1yr"), qcew_2yr = est("table3_4_qcew_2yr"),
            qwi_1yr = est("table5_6_qwi_1yr"), qwi_2yr = est("table7_8_qwi_2yr"))

# jobs lost: median percent times California fast-food employment in the base period
base_emp = c(read_t("tableA1_qcew_counts")$base_replicated[1], read_t("tableA1_qcew_counts")$base_replicated[1],
             read_t("tableA2_qwi_counts")$base_replicated[1], read_t("tableA2_qwi_counts")$base_replicated[1])
jobs = -med / 100 * base_emp

# wage effect: California minus Rest of US real weekly wage index, QCEW
w = read_t("figureA5_wage_index")
wage_dd = w$gap[match(c(4, 8, 4, 8), w$t)]
elasticity = med / wage_dd

out = data.frame(horizon = rownames(med), median_published = med[, 1], median_replicated = med[, 2],
                 jobs_published = round(jobs[, 1], -2), jobs_replicated = round(jobs[, 2], -2),
                 wage_effect = wage_dd, elasticity_published_median = elasticity[, 1],
                 elasticity_replicated = elasticity[, 2])
save_table(out, "medians_jobs_elasticities")
print(out, digits = 3)
cat("\nPaper: medians about -3.3 and -3.45 (QCEW), -1.38 and -3.52 (QWI)\n")
cat("Paper: about 18,000 and 20,000 jobs (QCEW), 8,000 and 20,000 (QWI)\n")
cat("Paper: wage gap about 8 percent after one year and just over 6 after two\n")
cat("Paper: elasticities -0.40 (QCEW 1 year), -0.17 (QWI 1 year), just over -0.5 (2 years)\n")
