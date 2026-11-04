# Published numbers from Clemens, Edwards, Meer and Nguyen (NBER WP 34033, October 2026)

# Panel A rows: group, industry, then the six columns (pre, base, post, trend, change, detrended)
panel_a_pub = function(txt) read.table(text = txt, col.names = c("group", "industry", "pre", "base",
                                       "post", "trend", "change", "detrended"))
# Panel B rows: industry, group, then pretrend, simple and detrended
panel_b_pub = function(txt) read.table(text = txt, col.names = c("industry", "group", "pretrend",
                                       "simple", "detrended"))
# placebo tables: spec, control, California estimate, p-value, number of placebo states
placebo_pub = function(txt) read.table(text = txt, col.names = c("spec", "control", "est", "p", "n"))

pub = list()

pub$t1a = panel_a_pub("
CA FF 92.48 100 97.26 3.76 -2.74 -6.50
CA HWD 95.03 100 100.48 2.48 0.48 -2.01
RoU FF 93.46 100 100.15 3.27 0.15 -3.12
RoU HWD 93.33 100 101.18 3.33 1.18 -2.15
NI FF 93.18 100 100.49 3.41 0.49 -2.92
NI HWD 93.01 100 101.17 3.49 1.17 -2.32")
pub$t1b = panel_b_pub("
HWD RoU -0.85 -0.70 0.14
HWD NI -1.01 -0.69 0.31
FF RoU 0.49 -2.89 -3.38
FF NI 0.35 -3.23 -3.58
DDD RoU 1.34 -2.19 -3.52
DDD NI 1.36 -2.54 -3.90")
pub$t2 = placebo_pub("
DD RoU -2.889 0.0204 48
DD NI -3.231 0.0435 22
DDD RoU -2.186 0.0816 48
DDD NI -2.537 0.0870 22
DetDD RoU -3.379 0.0612 48
DetDD NI -3.581 0.0435 22
DetDDD RoU -3.524 0.1224 48
DetDDD NI -3.895 0.1304 22")

pub$t3a = panel_a_pub("
CA FF 92.48 100 97.07 3.76 -2.93 -10.46
CA HWD 95.03 100 100.75 2.48 0.75 -4.22
RoU FF 93.46 100 99.80 3.27 -0.20 -6.74
RoU HWD 93.33 100 101.47 3.33 1.47 -5.20
NI FF 93.18 100 100.25 3.41 0.25 -6.58
NI HWD 93.01 100 101.62 3.49 1.62 -5.37")
pub$t3b = panel_b_pub("
HWD RoU -0.85 -0.72 0.98
HWD NI -1.01 -0.87 1.15
FF RoU 0.49 -2.74 -3.72
FF NI 0.35 -3.18 -3.88
DDD RoU 1.34 -2.02 -4.69
DDD NI 1.36 -2.31 -5.03")
pub$t4 = placebo_pub("
DD RoU -2.736 0.1020 48
DD NI -3.180 0.0435 22
DDD RoU -2.018 0.1633 48
DDD NI -2.310 0.1739 22
DetDD RoU -3.717 0.1224 48
DetDD NI -3.879 0.0870 22
DetDDD RoU -4.694 0.1224 48
DetDDD NI -5.026 0.1739 22")

pub$t5a = panel_a_pub("
CA FF 91.19 100 99.93 4.40 -0.07 -4.47
CA HWD 94.91 100 100.59 2.54 0.59 -1.95
RoU FF 92.55 100 100.79 3.72 0.79 -2.93
RoU HWD 93.12 100 101.27 3.44 1.27 -2.17
NI FF 92.80 100 101.15 3.60 1.15 -2.45
NI HWD 92.80 100 101.22 3.60 1.22 -2.38")
pub$t5b = panel_b_pub("
HWD RoU -0.89 -0.68 0.22
HWD NI -1.05 -0.62 0.43
FF RoU 0.68 -0.86 -1.54
FF NI 0.80 -1.22 -2.02
DDD RoU 1.58 -0.18 -1.75
DDD NI 1.86 -0.59 -2.45")
pub$t6 = placebo_pub("
DD RoU -0.856 0.3673 48
DD NI -1.216 0.3043 22
DDD RoU -0.178 0.4082 48
DDD NI -0.591 0.3913 22
DetDD RoU -1.537 0.2245 48
DetDD NI -2.017 0.0870 22
DetDDD RoU -1.754 0.2449 48
DetDDD NI -2.446 0.2174 22")

pub$t7a = panel_a_pub("
CA FF 91.19 100 98.33 4.40 -1.67 -10.47
CA HWD 94.91 100 100.90 2.54 0.90 -4.19
RoU FF 92.55 100 100.97 3.72 0.97 -6.48
RoU HWD 93.12 100 101.99 3.44 1.99 -4.89
NI FF 92.80 100 101.37 3.60 1.37 -5.83
NI HWD 92.80 100 102.10 3.60 2.10 -5.10")
pub$t7b = panel_b_pub("
HWD RoU -0.89 -1.09 0.70
HWD NI -1.05 -1.20 0.90
FF RoU 0.68 -2.63 -4.00
FF NI 0.80 -3.04 -4.64
DDD RoU 1.58 -1.54 -4.69
DDD NI 1.86 -1.83 -5.55")
pub$t8 = placebo_pub("
DD RoU -2.633 0.0816 48
DD NI -3.038 0.0435 22
DDD RoU -1.542 0.2653 48
DDD NI -1.835 0.3043 22
DetDD RoU -3.996 0.0816 48
DetDD NI -4.642 0.0435 22
DetDDD RoU -4.693 0.0816 48
DetDDD NI -5.545 0.0870 22")

pub$ta3a = panel_a_pub("
CA FF 92.82 100 99.13 3.59 -0.87 -4.46
CA HWD 96.56 100 100.67 1.72 0.67 -1.05
RoU FF 93.18 100 100.27 3.41 0.27 -3.14
RoU HWD 94.23 100 101.22 2.88 1.22 -1.66
NI FF 92.78 100 100.69 3.61 0.69 -2.93
NI HWD 93.88 100 101.08 3.06 1.08 -1.98")
pub$ta3b = panel_b_pub("
HWD RoU -1.16 -0.55 0.61
HWD NI -1.34 -0.40 0.94
FF RoU 0.18 -1.14 -1.32
FF NI -0.02 -1.56 -1.53
DDD RoU 1.34 -0.59 -1.93
DDD NI 1.32 -1.15 -2.47")
pub$ta4 = placebo_pub("
DD RoU -1.139 0.2245 48
DD NI -1.557 0.2174 22
DDD RoU -0.587 0.3061 48
DDD NI -1.152 0.3043 22
DetDD RoU -1.317 0.1837 48
DetDD NI -1.534 0.2174 22
DetDDD RoU -1.928 0.1837 48
DetDDD NI -2.471 0.1304 22")

# Tables A1 and A2: raw fast-food counts by region
pub$ta1 = read.table(text = "
CA 570909 555273
RoU 3901843 3907713
NI 1974813 1984533
Dube20 1902294 1912189", col.names = c("group", "base", "post"))
pub$ta2 = read.table(text = "
CA 549980 549612
RoU 3851097 3881467
NI 1939899 1962184
Dube20 1867203 1890328", col.names = c("group", "base", "post"))

# placebo states more negative than California, as listed in Tables 2, 4, 6, 8 and A4
pub$more_negative = list(
  t2 = list(DDD_RoU = c("10", "16", "36"), DDD_NI = "16", DetDD_RoU = c("10", "11"),
            DetDDD_RoU = c("10", "11", "16", "22", "36"), DetDDD_NI = c("16", "22")),
  t4 = list(DD_RoU = c("10", "11", "12", "30"), DDD_RoU = c("10", "12", "16", "30", "36", "45", "49"),
            DDD_NI = c("16", "45", "49"), DetDD_RoU = c("10", "11", "12", "22", "30"),
            DetDD_NI = "22", DetDDD_RoU = c("10", "11", "16", "22", "49"),
            DetDDD_NI = c("16", "22", "49")))
