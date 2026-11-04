# My extensions, all with the QCEW data and the paper's placebo inference
#   1. Full-service restaurants as the within-state control, the choice Sosinskiy and Reich make
#   2. Base month: enactment (Sep 2023), Dube's fourth quarter (Dec 2023), implementation (Mar 2024)
#   3. Synthetic control for California with two different pre-periods

source(here::here("code", "functions.R"))
library(quadprog)

m = read.csv(here("data", "clean", "qcew_monthly.csv"), colClasses = c(state = "character"))
by_month = function(mo) {
  x = m[m$month == mo, ]
  x$period = x$year
  x
}
p_of = function(res, spec, control) res$placebo$p[res$placebo$spec == spec & res$placebo$control == control]

# 1. full-service restaurants
d = by_month(9)
rows = list()
for (h in 1:2) {
  hw = build_table(d, 2021, 2023, 2023 + h, h)
  fs = build_table(d, 2021, 2023, 2023 + h, h, within = "fullservice")
  # full-service on its own, with full-service playing the role of fast food
  fd = d[d$type != "fastfood", ]
  fd$type[fd$type == "fullservice"] = "fastfood"
  fo = build_table(fd, 2021, 2023, 2023 + h, h)
  for (g in c("RoU", "NI")) {
    rows[[length(rows) + 1]] = data.frame(horizon = h, control = g,
      estimator = c("Fast food DD", "Full-service DD", "DDD, higher wage industries (paper)",
                    "DDD, full-service restaurants"),
      simple = c(hw$b[paste0("FF_", g), "simple"], fs$b[paste0("HWD_", g), "simple"],
                 hw$b[paste0("DDD_", g), "simple"], fs$b[paste0("DDD_", g), "simple"]),
      p_simple = c(p_of(hw, "DD", g), p_of(fo, "DD", g), p_of(hw, "DDD", g), p_of(fs, "DDD", g)),
      detrended = c(hw$b[paste0("FF_", g), "detrended"], fs$b[paste0("HWD_", g), "detrended"],
                    hw$b[paste0("DDD_", g), "detrended"], fs$b[paste0("DDD_", g), "detrended"]),
      p_detrended = c(p_of(hw, "DetDD", g), p_of(fo, "DetDD", g), p_of(hw, "DetDDD", g), p_of(fs, "DetDDD", g)))
  }
}
ext1 = do.call(rbind, rows)
save_table(ext1, "ext1_fullservice")
cat("Extension 1: full-service restaurants as the control industry\n")
print(ext1, digits = 3)

# 2. base month
bases = data.frame(label = c("Sep 2023 (enactment, paper)", "Dec 2023 (Dube)", "Mar 2024 (just before $20)"),
                   year = c(2023, 2023, 2024), month = c(9, 12, 3))
ext2 = do.call(rbind, lapply(seq_len(nrow(bases)), function(i) {
  b = bases[i, ]
  x = by_month(b$month)
  do.call(rbind, lapply(1:2, function(h) {
    if (b$year + h > 2025) return(NULL)
    r = build_table(x, b$year - 2, b$year, b$year + h, h)
    data.frame(base = b$label, horizon = h, dd_rou = r$b["FF_RoU", "simple"], p_dd_rou = p_of(r, "DD", "RoU"),
               dd_ni = r$b["FF_NI", "simple"], p_dd_ni = p_of(r, "DD", "NI"),
               ddd_rou = r$b["DDD_RoU", "simple"], p_ddd_rou = p_of(r, "DDD", "RoU"))
  }))
}))
save_table(ext2, "ext2_base_month")
cat("\nExtension 2: base month\n")
print(ext2, digits = 3)

# 3. synthetic control
ff = m[m$type == "fastfood", ]
ff$t = (ff$year - 2023) * 12 + ff$month - 9
pool = qualify(d[d$type == "fastfood", ], setdiff(unique(d$state), c(ca, dropped)))
wide = reshape(ff[ff$state %in% c(ca, pool), c("state", "t", "emp")], idvar = "t", timevar = "state",
               direction = "wide")
wide = wide[order(wide$t), ]
tt = wide$t
Y = as.matrix(wide[, -1])
colnames(Y) = sub("emp.", "", colnames(Y))
rownames(Y) = NULL
Y = 100 * sweep(Y, 2, Y[tt == 0, ], "/")

synth = function(target, donors, pre) {
  X = Y[pre, donors]
  D = crossprod(X)
  D = D + diag(1e-8 * mean(diag(D)), ncol(X))
  w = solve.QP(D, crossprod(X, Y[pre, target]), cbind(1, diag(ncol(X))), c(1, rep(0, ncol(X))), meq = 1)$solution
  w = pmax(w, 0)
  w = w / sum(w)
  gap = Y[, target] - Y[, donors] %*% w
  list(w = setNames(w, donors), gap = drop(gap), rmspe_pre = sqrt(mean(gap[pre]^2)),
       rmspe_post = sqrt(mean(gap[tt > 0]^2)))
}
pres = list("Sep 2021 to Sep 2023" = tt >= -24 & tt <= 0, "Jan 2015 to Sep 2023" = tt <= 0)
ext3 = list()
paths = list()
for (k in names(pres)) {
  s = synth(ca, pool, pres[[k]])
  plac = sapply(pool, function(j) {
    r = synth(j, setdiff(pool, j), pres[[k]])
    c(g12 = r$gap[tt == 12], g24 = r$gap[tt == 24], ratio = r$rmspe_post / r$rmspe_pre)
  })
  top = sort(s$w[s$w > 0.01], decreasing = TRUE)
  ext3[[k]] = data.frame(pre_period = k, gap_1yr = s$gap[tt == 12], p_1yr = placebo_p(s$gap[tt == 12], plac["g12", ]),
    gap_2yr = s$gap[tt == 24], p_2yr = placebo_p(s$gap[tt == 24], plac["g24", ]),
    rmspe_pre = s$rmspe_pre, p_rmspe_ratio = (sum(plac["ratio", ] >= s$rmspe_post / s$rmspe_pre) + 1) / (ncol(plac) + 1),
    weights = paste(sprintf("%s %.0f%%", state_names[names(top)], 100 * top), collapse = ", "))
  paths[[k]] = data.frame(pre_period = k, t = tt, california = Y[, ca], synthetic = Y[, ca] - s$gap)
}
ext3 = do.call(rbind, ext3)
save_table(ext3, "ext3_synthetic_control")
paths = do.call(rbind, paths)
save_table(paths, "ext3_synthetic_paths")
cat("\nExtension 3: synthetic control\n")
print(ext3[, -ncol(ext3)], digits = 3)
cat(paste(ext3$pre_period, ext3$weights, sep = ": "), sep = "\n")

pdf(here("results", "figures", "ext_synthetic.pdf"), width = 9, height = 3.6)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), mgp = c(2.3, 0.7, 0), cex.axis = 0.8)
for (k in names(pres)) {
  p = paths[paths$pre_period == k & paths$t >= -48, ]
  plot(p$t, p$california, type = "l", col = "#1f4e9c", lwd = 2, ylim = range(c(p$california, p$synthetic)),
       xlab = "Months since September 2023", ylab = "Index (Sep 2023 = 100)",
       main = paste("Pre-period", k), font.main = 1, cex.main = 0.95)
  lines(p$t, p$synthetic, col = "#c0392b", lwd = 2, lty = 2)
  abline(v = c(0, 7), lty = 3, col = c("black", "grey50"))
  legend("bottomleft", c("California", "Synthetic California"), col = c("#1f4e9c", "#c0392b"), lty = 1:2,
         lwd = 2, bty = "n", cex = 0.8)
}
dev.off()

# all one-year estimates in one picture
t12 = read.csv(here("results", "tables", "table1_2_qcew_1yr_placebo.csv"))
lab = c(DD = "DD", DDD = "DDD", DetDD = "Detrended DD", DetDDD = "Detrended DDD")
all1 = rbind(
  data.frame(group = "Paper, 8 estimates", label = paste0(lab[t12$spec], ", ", ifelse(t12$control == "RoU", "all states", "non-increasers")),
             est = t12$est, p = t12$p),
  data.frame(group = "Full-service control", label = c("DDD with full-service, all states", "DDD with full-service, non-increasers"),
             est = ext1$simple[ext1$horizon == 1 & ext1$estimator == "DDD, full-service restaurants"],
             p = ext1$p_simple[ext1$horizon == 1 & ext1$estimator == "DDD, full-service restaurants"]),
  data.frame(group = "Base month", label = paste("DD, base", c("Dec 2023", "Mar 2024")),
             est = ext2$dd_rou[ext2$horizon == 1][2:3], p = ext2$p_dd_rou[ext2$horizon == 1][2:3]),
  data.frame(group = "Synthetic control", label = paste("Synthetic, pre", names(pres)), est = ext3$gap_1yr, p = ext3$p_1yr))
save_table(all1, "ext_all_one_year")
pdf(here("results", "figures", "ext_all_estimates.pdf"), width = 8, height = 4.6)
par(mar = c(4, 17, 1, 1), mgp = c(2.3, 0.7, 0), cex.axis = 0.8)
y = rev(seq_len(nrow(all1)))
cols = c("Paper, 8 estimates" = "#1f4e9c", "Full-service control" = "#c0392b", "Base month" = "#7d6608",
         "Synthetic control" = "#117a65")[all1$group]
plot(all1$est, y, pch = ifelse(all1$p < 0.1, 19, 1), col = cols, xlim = c(min(all1$est) - 0.5, 1), yaxt = "n",
     ylab = "", xlab = "Effect on fast-food employment after one year (percent)", cex = 1.3)
axis(2, at = y, labels = all1$label, las = 1, cex.axis = 0.75)
abline(v = 0, col = "grey40")
abline(v = median(t12$est), lty = 3, col = "#1f4e9c")
legend("bottomright", c("p < 0.10", "p >= 0.10"), pch = c(19, 1), bty = "n", cex = 0.8)
dev.off()
