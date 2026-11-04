# Figures 1, 2, A2, A4 and A5 of the paper, plus two placebo pictures for the slides

source(here::here("code", "functions.R"))

blue = "#1f4e9c"
red = "#c0392b"
m = read.csv(here("data", "clean", "qcew_monthly.csv"), colClasses = c(state = "character"))
q = read.csv(here("data", "clean", "qwi_quarterly.csv"), colClasses = c(state = "character"))
rou_pool = function(d, type, by) {
  x = d[d$type == type, ]
  x = x[if (by == "month") x$month == 9 else x$year * 10 + x$qtr <= 20253, ]
  x$period = if (by == "month") x$year else x$year * 10 + x$qtr
  qualify(x, setdiff(unique(d$state), c(ca, dropped)))
}

# California and Rest of US series with event time t (0 = September 2023 or 2023Q3)
series = function(d, type, by, from, to) {
  x = d[d$type == type, ]
  x$t = if (by == "month") (x$year - 2023) * 12 + x$month - 9 else (x$year - 2023) * 4 + x$qtr - 3
  x$season = if (by == "month") x$month else x$qtr
  x = x[x$t >= from & x$t <= to, ]
  pool = rou_pool(d, type, by)
  sum_na = function(v) if (anyNA(v)) NA else sum(v)
  rou = aggregate(emp ~ t + season, data = x[x$state %in% pool, ], FUN = sum_na, na.action = na.pass)
  cal = x[x$state == ca, c("t", "season", "emp")]
  out = rbind(data.frame(group = "CA", cal), data.frame(group = "RoU", rou))
  out = out[order(out$group, out$t), ]
  out$index = ave(seq_len(nrow(out)), out$group, FUN = function(i) 100 * out$emp[i] / out$emp[i][out$t[i] == 0])
  # detrend with the pre-period slope, then remove pre-period seasonal means
  start = ave(seq_len(nrow(out)), out$group, FUN = function(i) out$index[i][out$t[i] == from])
  out$detrended = out$index - (100 + (100 - start) / abs(from) * out$t)
  pre = out[out$t <= 0, ]
  s = aggregate(detrended ~ group + season, data = pre, FUN = mean)
  out$deseasoned = out$detrended - s$detrended[match(paste(out$group, out$season), paste(s$group, s$season))]
  out
}

four_panel = function(s, file, unit, xlab_at, xlab, vlines, ylab_level) {
  pdf(here("results", "figures", file), width = 9, height = 6.2)
  par(mfrow = c(2, 2), mar = c(3, 4.2, 2, 4.2), mgp = c(2.4, 0.7, 0), cex.axis = 0.8, cex.lab = 0.85)
  ca_s = s[s$group == "CA", ]
  ro_s = s[s$group == "RoU", ]
  sf = mean(ca_s$emp, na.rm = TRUE) / mean(ro_s$emp, na.rm = TRUE)
  draw = function(y, ylab, title, hline = NULL, scale_rou = 1) {
    y1 = ca_s[[y]] / ifelse(y == "emp", 1000, 1)
    y2 = ro_s[[y]] * scale_rou / ifelse(y == "emp", 1000, 1)
    plot(ca_s$t, y1, type = "n", ylim = range(c(y1, y2), na.rm = TRUE), xaxt = "n", xlab = "", ylab = ylab,
         main = title, font.main = 1, cex.main = 0.95)
    axis(1, at = xlab_at, labels = xlab)
    abline(v = vlines, lty = 3, col = c("black", "grey50"))
    if (!is.null(hline)) abline(h = hline, col = "grey40")
    lines(ca_s$t, y1, col = blue, lwd = 2)
    lines(ro_s$t, y2, col = red, lwd = 2, lty = 2)
  }
  draw("emp", paste("California", ylab_level), "(a) Levels", scale_rou = sf)
  at = pretty(ca_s$emp / 1000)
  axis(4, at = at, labels = format(round(at / sf)))
  mtext(paste("Rest of US", ylab_level), side = 4, line = 2.4, cex = 0.7)
  legend("bottomright", c("California", "Rest of US"), col = c(blue, red), lty = 1:2, lwd = 2, bty = "n", cex = 0.8)
  draw("index", paste0("Index (", unit, " = 100)"), "(b) Indexed", hline = 100)
  draw("detrended", "Index, deviation from trend", "(c) Indexed and detrended", hline = 0)
  draw("deseasoned", "Index, residual", "(d) Detrended and deseasoned", hline = 0)
  dev.off()
}

mlab = seq(-24, 24, by = 6)
mnames = c("Sep 21", "Mar 22", "Sep 22", "Mar 23", "Sep 23", "Mar 24", "Sep 24", "Mar 25", "Sep 25")
qlab = seq(-8, 8, by = 2)
qnames = paste0(rep(c("Q3", "Q1"), length.out = 9), " ", c(21, 22, 22, 23, 23, 24, 24, 25, 25))

f1 = series(m, "fastfood", "month", -24, 24)
four_panel(f1, "figure1_qcew_fastfood.pdf", "Sep 2023", mlab, mnames, c(0, 7), "(thousands)")
fa4 = series(m, "hwd", "month", -24, 24)
four_panel(fa4, "figureA4_qcew_hwd.pdf", "Sep 2023", mlab, mnames, c(0, 7), "(thousands)")
f2 = series(q, "fastfood", "quarter", -8, 8)
four_panel(f2, "figure2_qwi_fastfood.pdf", "2023Q3", qlab, qnames, c(0, 3), "(thousands)")
fa2 = series(q, "hwd", "quarter", -8, 8)
four_panel(fa2, "figureA2_qwi_hwd.pdf", "2023Q3", qlab, qnames, c(0, 3), "(thousands)")
save_table(rbind(data.frame(figure = "1", f1), data.frame(figure = "2", f2)), "figure1_2_series")

# Figure A5: real average weekly wage in fast food, 2023Q3 = 100
qc = read.csv(here("data", "clean", "qcew_quarterly.csv"), colClasses = c(state = "character"))
cpi = read.csv(here("data", "raw", "cpi", "CPIAUCNS.csv"))
cpi$year = as.integer(substr(cpi[[1]], 1, 4))
cpi$qtr = (as.integer(substr(cpi[[1]], 6, 7)) - 1) %/% 3 + 1
cpi = aggregate(CPIAUCNS ~ year + qtr, data = cpi, FUN = mean)
w = qc[qc$type == "fastfood", ]
w$t = (w$year - 2023) * 4 + w$qtr - 3
w = w[w$t >= -8 & w$t <= 8, ]
pool = rou_pool(m, "fastfood", "month")
wage = function(s) {
  x = w[w$state %in% s & !is.na(w$wages), ]
  a = aggregate(cbind(wages, avg_emp) ~ t + year + qtr, data = x, FUN = sum)
  a$real = a$wages / (13 * a$avg_emp) / cpi$CPIAUCNS[match(paste(a$year, a$qtr), paste(cpi$year, cpi$qtr))]
  a$index = 100 * a$real / a$real[a$t == 0]
  a[order(a$t), ]
}
wca = wage(ca)
wro = wage(pool)
wage_dd = data.frame(t = wca$t, california = wca$index, rest_of_us = wro$index, gap = wca$index - wro$index)
save_table(wage_dd, "figureA5_wage_index")
pdf(here("results", "figures", "figureA5_wages.pdf"), width = 7, height = 4.2)
par(mar = c(4, 4.2, 1, 1), mgp = c(2.4, 0.7, 0))
plot(wca$t, wca$index, type = "n", ylim = c(90, 110), xlab = "Quarters since September 2023",
     ylab = "Real weekly wage index (2023Q3 = 100)", xaxt = "n")
axis(1, at = -8:8)
abline(v = c(0, 3), lty = 3, col = c("black", "grey50"))
abline(h = 100, col = "grey40")
lines(wca$t, wca$index, col = blue, lwd = 2)
lines(wro$t, wro$index, col = red, lwd = 2, lty = 2)
legend("topleft", c("California", "Rest of US"), col = c(blue, red), lty = 1:2, lwd = 2, bty = "n")
dev.off()

# placebo picture: California against each of the 48 placebo states, simple DD, 1 year and 2 years
draws = list("One year (Sep 2023 to Sep 2024)" = read.csv(here("results", "tables", "table1_2_qcew_1yr_placebo_draws.csv")),
             "Two years (Sep 2023 to Sep 2025)" = read.csv(here("results", "tables", "table3_4_qcew_2yr_placebo_draws.csv")))
ca_est = c(read.csv(here("results", "tables", "table1_2_qcew_1yr_placebo.csv"))$est[1],
           read.csv(here("results", "tables", "table3_4_qcew_2yr_placebo.csv"))$est[1])
pdf(here("results", "figures", "placebo_qcew.pdf"), width = 9, height = 3.6)
par(mfrow = c(1, 2), mar = c(4, 1, 2, 1), mgp = c(2.3, 0.7, 0))
for (k in 1:2) {
  x = draws[[k]]
  x = x[x$estimator == "FF_RoU", ]
  v = sort(x$simple)
  plot(v, seq_along(v), pch = 19, col = "grey55", xlim = range(c(v, ca_est[k])), yaxt = "n", ylab = "",
       xlab = "Placebo DD estimate (index points)", main = names(draws)[k], font.main = 1, cex.main = 0.95)
  abline(v = 0, col = "grey40")
  abline(v = ca_est[k], col = blue, lwd = 2)
  text(ca_est[k], length(v) * 0.92, sprintf("California %.2f", ca_est[k]), pos = 4, col = blue, cex = 0.8)
  low = order(x$simple)[seq_len(sum(x$simple < ca_est[k]))]
  if (length(low)) text(x$simple[low], match(x$simple[low], v), state_names[pad2(x$state[low])], pos = 4, cex = 0.65)
}
dev.off()

# gap over time: California minus Rest of US, with every placebo state minus the rest in grey
ff = m[m$type == "fastfood", ]
ff$t = (ff$year - 2023) * 12 + ff$month - 9
ff = ff[ff$t >= -24, ]
wide = reshape(ff[, c("state", "t", "emp")], idvar = "t", timevar = "state", direction = "wide")
wide = wide[order(wide$t), ]
colnames(wide) = sub("emp.", "", colnames(wide))
idx = function(v) 100 * v / v[wide$t == 0]
gap = function(s, pool) idx(wide[[s]]) - idx(rowSums(wide[, setdiff(pool, s), drop = FALSE]))
gaps = sapply(pool, function(s) gap(s, pool))
ca_gap = idx(wide[[ca]]) - idx(rowSums(wide[, pool]))
save_table(data.frame(t = wide$t, california_gap = ca_gap), "gap_path_qcew")
pdf(here("results", "figures", "gap_paths_qcew.pdf"), width = 8, height = 4.2)
par(mar = c(4, 4.2, 1, 1), mgp = c(2.4, 0.7, 0))
matplot(wide$t, gaps, type = "l", lty = 1, col = adjustcolor("grey50", 0.35), ylim = c(-8, 8), xaxt = "n",
        xlab = "", ylab = "State minus rest, index points (Sep 2023 = 0)")
axis(1, at = c(seq(-24, 24, by = 6), 27), labels = c(mnames, "Dec 25"))
abline(h = 0, col = "grey30")
abline(v = c(0, 7), lty = 3, col = c("black", "grey50"))
lines(wide$t, ca_gap, col = blue, lwd = 3)
legend("bottomleft", c("California", "Each placebo state"), col = c(blue, "grey50"), lwd = c(3, 1), bty = "n")
dev.off()
cat("Figures written. Wage gap at 2024Q3 and 2025Q3:", round(wage_dd$gap[wage_dd$t %in% c(4, 8)], 2), "\n")
