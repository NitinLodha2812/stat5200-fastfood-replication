# Builds the state by month QCEW panel from the raw API files
#   fastfood = limited-service restaurants (722513), fullservice = 722511
#   hwd = higher wage industries = total private minus accommodation and food (72) minus retail (44-45)

source(here::here("code", "functions.R"))

types = c("10" = "total", "44_45" = "retail", "72" = "food", "722511" = "fullservice",
          "722513" = "fastfood")

read_one = function(f) {
  x = read.csv(f, colClasses = "character")
  x = x[x$own_code == "5", ]
  code = sub("^[0-9]{4}_q[0-9]_(.*)[.]csv$", "\\1", basename(f))
  # suppressed cells are written as 0 by BLS, so they become missing
  num = function(v) ifelse(x$disclosure_code != "", NA, as.numeric(v))
  data.frame(state = substr(x$area_fips, 1, 2), type = types[[code]],
             year = as.integer(x$year), qtr = as.integer(x$qtr),
             m1 = num(x$month1_emplvl), m2 = num(x$month2_emplvl), m3 = num(x$month3_emplvl),
             wages = num(x$total_qtrly_wages))
}
q = do.call(rbind, lapply(list.files(here("data", "raw", "qcew"), full.names = TRUE), read_one))
q = q[!q$state %in% territories, ]
stopifnot(!anyDuplicated(q[, c("state", "type", "year", "qtr")]))

# higher wage industries, missing if any piece is missing
key = paste(q$state, q$year, q$qtr)
pick = function(t, v) q[[v]][q$type == t][match(key[q$type == "total"], key[q$type == t])]
tot = q[q$type == "total", ]
hwd = tot
hwd$type = "hwd"
for (v in c("m1", "m2", "m3", "wages")) hwd[[v]] = tot[[v]] - pick("food", v) - pick("retail", v)
q = rbind(q, hwd)

# monthly employment
monthly = do.call(rbind, lapply(1:3, function(k)
  data.frame(state = q$state, type = q$type, year = q$year,
             month = (q$qtr - 1) * 3 + k, emp = q[[paste0("m", k)]])))
monthly = monthly[order(monthly$type, monthly$state, monthly$year, monthly$month), ]
write.csv(monthly, here("data", "clean", "qcew_monthly.csv"), row.names = FALSE)

# quarterly average employment and wage bill, for the wage figure
quarterly = data.frame(q[, c("state", "type", "year", "qtr")],
                       avg_emp = (q$m1 + q$m2 + q$m3) / 3, wages = q$wages)
write.csv(quarterly, here("data", "clean", "qcew_quarterly.csv"), row.names = FALSE)

cat("QCEW panel:", length(unique(monthly$state)), "states,", min(q$year), "to", max(q$year), "\n")
cat("Missing September values by type:\n")
sep = monthly[monthly$month == 9, ]
print(tapply(is.na(sep$emp), sep$type, sum))
cat("California fast food, September 2023:", sep$emp[sep$state == ca & sep$type == "fastfood" & sep$year == 2023], "\n")
