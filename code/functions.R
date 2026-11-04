# Settings and helper functions shared by the scripts

library(here)

dir.create(here("data", "clean"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("results", "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("results", "figures"), showWarnings = FALSE, recursive = TRUE)

ca = "06"
dropped = c("02", "26")      # Alaska and Michigan, incomplete in the QWI
territories = c("72", "78")
# the 22 states that did not raise their minimum wage in the sample period
non_increasers = c("01", "05", "13", "16", "18", "19", "20", "21", "22", "28", "33",
                   "37", "38", "40", "42", "45", "47", "48", "49", "54", "55", "56")

state_names = c("01" = "Alabama", "04" = "Arizona", "05" = "Arkansas", "06" = "California",
  "08" = "Colorado", "09" = "Connecticut", "10" = "Delaware", "11" = "District of Columbia",
  "12" = "Florida", "13" = "Georgia", "15" = "Hawaii", "16" = "Idaho", "17" = "Illinois",
  "18" = "Indiana", "19" = "Iowa", "20" = "Kansas", "21" = "Kentucky", "22" = "Louisiana",
  "23" = "Maine", "24" = "Maryland", "25" = "Massachusetts", "27" = "Minnesota",
  "28" = "Mississippi", "29" = "Missouri", "30" = "Montana", "31" = "Nebraska", "32" = "Nevada",
  "33" = "New Hampshire", "34" = "New Jersey", "35" = "New Mexico", "36" = "New York",
  "37" = "North Carolina", "38" = "North Dakota", "39" = "Ohio", "40" = "Oklahoma",
  "41" = "Oregon", "42" = "Pennsylvania", "44" = "Rhode Island", "45" = "South Carolina",
  "46" = "South Dakota", "47" = "Tennessee", "48" = "Texas", "49" = "Utah", "50" = "Vermont",
  "51" = "Virginia", "53" = "Washington", "54" = "West Virginia", "55" = "Wisconsin",
  "56" = "Wyoming")

pad2 = function(x) formatC(as.integer(x), width = 2, flag = "0")

# states with a value in every period, as the paper requires for its balanced panel
qualify = function(d, states) {
  d = d[d$state %in% states, ]
  n = tapply(!is.na(d$emp), d$state, sum)
  names(n)[n == length(unique(d$period))]
}

# sum employment over states for the chosen periods, then index to the base period
index_of = function(d, states, periods, base) {
  s = sapply(periods, function(p) sum(d$emp[d$state %in% states & d$period == p]))
  100 * s / s[periods == base]
}

# one Panel A row: pre, base, post, annual pre-trend, change and detrended change
panel_a = function(idx, years_out) {
  trend = (idx[2] - idx[1]) / 2
  change = idx[3] - idx[2]
  c(pre = idx[[1]], base = idx[[2]], post = idx[[3]], trend = trend[[1]],
    change = change[[1]], detrended = change[[1]] - years_out * trend[[1]])
}

dd = function(a, b) c(pretrend = a[["trend"]] - b[["trend"]], simple = a[["change"]] - b[["change"]],
                      detrended = a[["detrended"]] - b[["detrended"]])

# one-sided placebo p-value with California counted as one draw
placebo_p = function(est_ca, est_placebo) (sum(est_placebo < est_ca) + 1) / (length(est_placebo) + 1)

save_table = function(x, name) write.csv(x, here("results", "tables", paste0(name, ".csv")), row.names = FALSE)

# Panel A rows, Panel B estimates and placebo tests for one table of the paper
# d has columns state, type, period, emp and covers the whole window used to balance the panel
build_table = function(d, pre, base, post, years_out, within = "hwd") {
  periods = c(pre, base, post)
  ff = d[d$type == "fastfood", ]
  hw = d[d$type == within, ]
  row = function(x, states) panel_a(index_of(x, states, periods, base), years_out)
  sets = list(RoU = setdiff(unique(d$state), c(ca, dropped)), NI = setdiff(non_increasers, dropped))
  a = list(CA_FF = row(ff, ca), CA_HWD = row(hw, ca))
  b = list()
  plac = list()
  for (g in names(sets)) {
    pf = qualify(ff, sets[[g]])
    ph = qualify(hw, sets[[g]])
    a[[paste0(g, "_FF")]] = row(ff, pf)
    a[[paste0(g, "_HWD")]] = row(hw, ph)
    b[[paste0("HWD_", g)]] = dd(a$CA_HWD, a[[paste0(g, "_HWD")]])
    b[[paste0("FF_", g)]] = dd(a$CA_FF, a[[paste0(g, "_FF")]])
    b[[paste0("DDD_", g)]] = b[[paste0("FF_", g)]] - b[[paste0("HWD_", g)]]
    # placebo: each control state in turn is treated, the rest of the pool is the control
    plac[[paste0("FF_", g)]] = sapply(pf, function(s) dd(row(ff, s), row(ff, setdiff(pf, s))))
    pd = intersect(pf, ph)
    plac[[paste0("DDD_", g)]] = sapply(pd, function(s)
      dd(row(ff, s), row(ff, setdiff(pf, s))) - dd(row(hw, s), row(hw, setdiff(ph, s))))
  }
  specs = expand.grid(control = c("RoU", "NI"), est = c("FF", "DDD"), metric = c("simple", "detrended"),
                      stringsAsFactors = FALSE)
  p = do.call(rbind, lapply(seq_len(nrow(specs)), function(i) {
    s = specs[i, ]
    ca_est = b[[paste0(s$est, "_", s$control)]][[s$metric]]
    dist = plac[[paste0(s$est, "_", s$control)]][s$metric, ]
    data.frame(spec = paste0(if (s$metric == "detrended") "Det" else "", if (s$est == "FF") "DD" else "DDD"),
               control = s$control, est = ca_est, p = placebo_p(ca_est, dist), n = length(dist),
               more_negative = paste(names(dist)[dist < ca_est], collapse = " "))
  }))
  list(a = do.call(rbind, a), b = do.call(rbind, b), placebo = p, dist = plac)
}

# published next to replicated, one row per number
compare = function(res, pa, pb, pp, table) {
  a_rows = paste(pa$group, pa$industry, sep = "_")
  a = expand.grid(row = a_rows, column = colnames(res$a), stringsAsFactors = FALSE)
  a$published = mapply(function(r, c) pa[a_rows == r, c], a$row, a$column)
  a$replicated = mapply(function(r, c) res$a[r, c], a$row, a$column)
  b_rows = paste(pb$industry, pb$group, sep = "_")
  b = expand.grid(row = b_rows, column = colnames(res$b), stringsAsFactors = FALSE)
  b$published = mapply(function(r, c) pb[b_rows == r, c], b$row, b$column)
  b$replicated = mapply(function(r, c) res$b[r, c], b$row, b$column)
  pr = paste(pp$spec, pp$control, sep = "_")
  p = rbind(data.frame(row = pr, column = "estimate", published = pp$est, replicated = res$placebo$est),
            data.frame(row = pr, column = "p_value", published = pp$p, replicated = res$placebo$p),
            data.frame(row = pr, column = "n_placebo", published = pp$n, replicated = res$placebo$n))
  out = rbind(data.frame(table = table, panel = "A", a), data.frame(table = table, panel = "B", b),
              data.frame(table = table, panel = "placebo", p))
  out$difference = out$replicated - out$published
  out
}

# saves the comparison, the placebo table with state names and the placebo distributions
report = function(res, cmp, name) {
  save_table(cmp, name)
  pl = res$placebo
  pl$more_negative = sapply(strsplit(pl$more_negative, " "), function(s) paste(state_names[s], collapse = ", "))
  save_table(pl, paste0(name, "_placebo"))
  dist = do.call(rbind, lapply(names(res$dist), function(k)
    data.frame(estimator = k, state = colnames(res$dist[[k]]), t(res$dist[[k]]))))
  save_table(dist, paste0(name, "_placebo_draws"))
  cat("\n", name, "\n")
  print(round(res$a, 2))
  print(round(res$b, 2))
  print(pl, digits = 4)
  for (p in c("A", "B", "placebo")) {
    x = cmp[cmp$panel == p & cmp$column != "n_placebo", ]
    cat(sprintf("Panel %s: largest gap %.3f, %d of %d within 0.01\n", p, max(abs(x$difference)),
                sum(abs(x$difference) <= 0.01 + 1e-9), nrow(x)))
  }
}
