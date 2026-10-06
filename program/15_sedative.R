## Sens - excl sedative

library(fst)
library(data.table)

source("program/00_config.R")
source("program/00_functions.R")

dx_labels <- c("F20-29" = "Schizophrenia-spectrum",
               "F30-31" = "Bipolar disorder",
               "F00-03" = "Dementia",
               "F70-79" = "Intellectual disability",
               "F-other" = "Other psychiatric disorders",
               "no-dx" = "No recorded psychiatric diagnosis",
               "Overall" = "Overall")

episodes <- read.fst(file.path(input_dir, "episodes.fst"), as.data.table = TRUE)
app60 <- read.fst(file.path(input_dir, "app_periods_60.fst"), as.data.table = TRUE)
obs_dx_tv  <- read_fst(file.path(input_dir, "obs_dx_tv.fst"), as.data.table = TRUE)


# Denominator
ap_years <- unique(
  split_by_year(episodes[, .(id, s = dup_start, e = dup_end)],
                "s", "e")[year %in% STUDY_YEARS, .(id, year)]
)

dx_years <- split_by_year(
  obs_dx_tv[, .(id, s = dx_start, e = dx_end, dx_grp)], "s", "e"
)[year %in% STUDY_YEARS]
dx_years[, dx := factor(dx_grp, levels = LEVELS)]
dx_year <- dx_years[, .(dx_grp = dx_grp[which.min(as.integer(dx_grp))]),
                    by = .(id, year)]

# Rebuild APP after removing sedatives
episodes[, is_sed := (atc %chin% SEDATIVE_AP) |
           (atc == QUETIAPINE & !is.na(avg_dose) & avg_dose < QUET_DOSE_THR)]
app_ns <- make_app_periods(episodes[is_sed == FALSE], APP_PRIMARY)

# Prevalence with and without sedative AP
prev_w <- prev_yr(make_yearly(app60, ap_years, dx_year, LEVELS))[, set := "With sedatives"]
prev_ns <- prev_yr(make_yearly(app_ns, ap_years, dx_year, LEVELS))[, set := "Without sedatives"]
prev <- rbind(prev_w, prev_ns)

# 2024 table
p24 <- prev[year == 2024]
p24[, cell := fifelse(n_app < 10L, "<10", sprintf("%.1f", prev))]
p24[, grp := factor(dx_labels[dx_grp], levels = unname(dx_labels))]
tabs6 <- dcast(p24, grp ~ set, value.var = "cell")
setorder(tabs6, grp)
setnames(tabs6, "grp", "Diagnostic group")
print(tabs6)

cat(sprintf("Overall 2024: with sedatives %.1f%%, without %.1f%%\n",
            prev_w[dx_grp == "Overall" & year == 2024, prev],
            prev_ns[dx_grp == "Overall" & year == 2024, prev]))

write.xlsx(tabs6, file.path(out_dir, "stable6_sedatives.xlsx"))

# Clean-up
rm(list = ls())
