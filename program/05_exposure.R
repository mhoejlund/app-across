## Building analytical dataset for APP across

library(fst)
library(data.table)

source("program/00_config.R")
source("program/00_functions.R")

episodes   <- read_fst(file.path(input_dir, "episodes.fst"),  as.data.table = TRUE)
obs_dx_tv  <- read_fst(file.path(input_dir, "obs_dx_tv.fst"), as.data.table = TRUE)
obs_dx_tv[, dx_grp := factor(dx_grp, levels = LEVELS)]


# APP Episodes at every threshold on full episodes
for (thr in APP_THRESHOLDS) {
  pr <- make_app_periods(episodes, thr)
  write.fst(pr, file.path(input_dir, sprintf("app_pairs_%d.fst", thr))) # Pair-level 
  write.fst(to_person(pr), file.path(input_dir, sprintf("app_periods_%d.fst", thr))) # Person-level
}
app_pairs60 <- read.fst(file.path(input_dir, "app_pairs_60.fst"), as.data.table = TRUE)
app60 <- read.fst(file.path(input_dir, "app_periods_60.fst"), as.data.table = TRUE)


# Sensitivity analysis any overlap included
app60_state <- make_app_periods_anyoverlap(episodes, 60L)
write.fst(app60_state, file.path(input_dir, "app_periods_state_60.fst"))


# APP episodes split by time-varying diagnosis
setkey(app60, id, app_start, app_end)
setkey(obs_dx_tv, id, dx_start, dx_end)
app_dx <- foverlaps(app60, obs_dx_tv,
                    by.x = c("id", "app_start", "app_end"),
                    by.y = c("id", "dx_start", "dx_end"),
                    type = "any",
                    nomatch = 0L
)

app_dx[, `:=` (
  seg_start = pmax(app_start, dx_start),
  seg_end   = pmin(app_end, dx_end),
  seg_days  = as.integer(pmin(app_end, dx_end, STUDY_END) - pmax(app_start, dx_start, STUDY_START) + 1L),
  fu_end = FU_END
)]
app_dx <- app_dx[seg_days > 0]
write.fst(app_dx, file.path(input_dir, "app_dx.fst"))


# Union of all AP exposure per person
ap_any <- union_intervals(episodes[, .(id, dup_start, dup_end)],
                          "id", "dup_start", "dup_end")
write.fst(ap_any, file.path(input_dir, "ap_any.fst"))


# Calendar year split
dx_y <- split_by_year(obs_dx_tv[, .(id, dx_start = as.Date(dx_start),
                                    dx_end = as.Date(dx_end), dx_grp)],
                      "dx_start", "dx_end")[ys < ye & year %in% STUDY_YEARS]
write.fst(dx_y, file.path(input_dir, "dx_y.fst"))

ap_y <- split_by_year(ap_any[, .(id, ap_start = as.Date(ap_start),
                                    ap_end = as.Date(ap_end))],
                      "ap_start", "ap_end")[ys < ye & year %in% STUDY_YEARS]
write.fst(ap_y, file.path(input_dir, "ap_y.fst"))


app_y <- split_by_year(app60[, .(id, app_start = as.Date(app_start),
                                 app_end = as.Date(app_end))],
                      "app_start", "app_end")[ys < ye & year %in% STUDY_YEARS]
write.fst(app_y, file.path(input_dir, "app_y.fst"))


# Aggregated AP/APP days per year x dx_grp
write.fst(calc_app_days(dx_y, ap_y), file.path(input_dir, "ap_days_year_dx.fst"))
write.fst(calc_app_days(dx_y, app_y), file.path(input_dir, "app_days_year_dx.fst"))


# Person-year cohort and yearly APP flags
ap_years <- unique(
  split_by_year(episodes[, .(id, s = dup_start, e = dup_end)],
                "s", "e")[year %in% STUDY_YEARS, .(id, year)]
)
dx_years <- split_by_year(obs_dx_tv[, .(id, s = dx_start, e = dx_end, dx_grp)],
                          "s", "e")[year %in% STUDY_YEARS]
dx_years[, dx_grp := factor(dx_grp, levels = LEVELS)]
dx_year <- dx_years[, .(dx_grp = dx_grp[which.min(as.integer(dx_grp))]),
                    by = .(id, year)]

yearly <- make_yearly(app60, ap_years, dx_year, LEVELS)
write.fst(yearly, file.path(input_dir, "yearly.fst"))


# Clean-up
rm(list = ls())
gc()
