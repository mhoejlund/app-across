## Compare APP definitions

library(fst)
library(data.table)

source("program/00_config.R")
source("program/00_functions.R")

episodes   <- read_fst(file.path(input_dir, "episodes.fst"),  as.data.table = TRUE)
obs_dx_tv  <- read_fst(file.path(input_dir, "obs_dx_tv.fst"), as.data.table = TRUE)
app60   <- read_fst(file.path(input_dir, "app_periods_60.fst"),  as.data.table = TRUE)
app60_state  <- read_fst(file.path(input_dir, "app_periods_state_60.fst"), as.data.table = TRUE)


# rebuild person-year cohort
ap_years <- unique(split_by_year(episodes[, .(id, s = dup_start, e = dup_end)], "s", "e")[year %in% STUDY_YEARS, .(id, year)])
dxy <- split_by_year(obs_dx_tv[, .(id, s = dx_start, e = dx_end, dx_grp)], "s", "e")[year %in% STUDY_YEARS]
dxy[, dx_grp := factor(dx_grp, levels = LEVELS)]
dx_year <- dxy[, .(dx_grp = dx_grp[which.min(as.integer(dx_grp))]), by = .(id, year)]

prev2024 <- function(app, label) {
  y <- make_yearly(app, ap_years, dx_year, LEVELS)
  rbind(y[year == 2024, .(prev = 100 * mean(app)), by = .(dx_grp = as.character(dx_grp))],
        y[year == 2024, .(dx_grp = "Overall", prev = 100 * mean(app))])[, def := label][]
}

cmp <- rbind(prev2024(app60, "pair"), prev2024(app60_state, "state"))
comparison <- dcast(cmp, dx_grp ~ def, value.var = "prev")[, diff := state - pair][]
print(comparison)

n_users <- uniqueN(ap_years$id)
cat(sprintf("Ever-APP pair %.1f%% state %.1f%%\n",
            100 * uniqueN(app60$id) / n_users, 100 * uniqueN(app60_state$id) / n_users))
