# Builds the state by quarter QWI panel of beginning-of-quarter employment (Emp)
# The window is 2015Q1 to 2025Q3, the quarters the authors extracted

source(here::here("code", "functions.R"))

types = c("00" = "total", "44-45" = "retail", "72" = "food", "722511" = "fullservice",
          "722513" = "fastfood")

raw = rbind(read.csv(here("data", "raw", "qwi", "qwi_states_ns.csv"), colClasses = "character"),
            read.csv(here("data", "raw", "qwi", "qwi_states_n6.csv"), colClasses = "character"))
q = data.frame(state = raw$geography, type = unname(types[raw$industry]),
               year = as.integer(raw$year), qtr = as.integer(raw$quarter),
               emp = suppressWarnings(as.numeric(raw$Emp)))
q = q[q$year * 10 + q$qtr <= 20253 & !q$state %in% territories, ]
stopifnot(!anyDuplicated(q[, c("state", "type", "year", "qtr")]))

# higher wage industries, missing if any piece is missing
key = paste(q$state, q$year, q$qtr)
tot = q[q$type == "total", ]
get = function(t) q$emp[q$type == t][match(key[q$type == "total"], key[q$type == t])]
hwd = tot
hwd$type = "hwd"
hwd$emp = tot$emp - get("food") - get("retail")
q = rbind(q, hwd)
q = q[order(q$type, q$state, q$year, q$qtr), ]
write.csv(q, here("data", "clean", "qwi_quarterly.csv"), row.names = FALSE)

cat("QWI panel:", length(unique(q$state)), "states, 2015Q1 to 2025Q3\n")
cat("States with any missing fast-food quarter:",
    unique(q$state[q$type == "fastfood" & is.na(q$emp)]), "\n")
cat("California fast food, 2023Q3:", q$emp[q$state == ca & q$type == "fastfood" & q$year == 2023 & q$qtr == 3], "\n")
