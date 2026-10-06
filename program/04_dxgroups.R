## Diagnostic grouping for APP across

library(fst)
library(data.table)

source("program/00_config.R")
source("program/00_functions.R")

# Observation windows clipped to study window/death ---------------------------
person <- read_fst(file.path(input_dir, "person.fst"), as.data.table = TRUE)
obs    <- read_fst(file.path(input_dir, "obs.fst"),    as.data.table = TRUE)

obs <- merge(obs[, .(id, obs_start, obs_end)], person, by = "id", all.x = TRUE)
obs[, `:=` (obs_start = as.Date(obs_start), obs_end = as.Date(obs_end), deathdate = as.Date(deathdate))]
obs <- obs[obs_end >= STUDY_START & obs_start <= STUDY_END]
obs[, obs_start := pmax(obs_start, STUDY_START)]
obs[, obs_end   := pmin(obs_end, STUDY_END, deathdate, na.rm = TRUE)]
obs <- obs[, .(id, obs_start, obs_end, sex, birthdate)]
write_fst(obs, file.path(input_dir, "obs_mod.fst"))


# Map diagnoses to groups -----------------------------------------------------
dx <- read_fst(file.path(input_dir, "dx.fst"), as.data.table = TRUE)
dx[, dxdate := as.Date(dxdate)]
dx <- dx[is.na(dxparent) | dxparent != "Z032"]
dx[, dx_grp := fcase(
  grepl("^29[578]|^F2[0-9]",                diag), "F20-29",
  grepl("^296|^F3[0-1]",                    diag), "F30-31",
  grepl("^290|^F0[0-3]|^G3[0-1]",           diag), "F00-03",
  grepl("^310[0-5]|^F7[0-9]",               diag), "F70-79",
  grepl("^29[1-4]|^30[0-9]|^F0[4-9]|^F10[1-9]|^F1[1-9]|^F3[2-9]|^F[456]|^F[89]", diag), "F-other", # Had missed to inlcude F10-19 in other :(
  default = NA_character_
)]
dx <- dx[dx_grp %in% PRIO][, .(id, dx_grp, dxdate)]
dx[, dx_grp := factor(dx_grp, levels = PRIO)]
setorder(dx, id, dxdate)


# Highest diagnosis over time -------------------------------------------------
# Per id/date: highest priority dx on that day
dx[, rank := as.integer(dx_grp)]
dx_day <- dx[, .(rank = min(rank)), by = .(id, dxdate)]
setorder(dx_day, id, dxdate)

# Running best over time
dx_day[, best := cummin(rank), by = id]
dx_day[, dx_grp := factor(PRIO[best], levels = PRIO)]

# Keep only change points
dx_best <- dx_day[, {
  keep <- c(TRUE, diff(best) != 0)
  .(dxdate = dxdate[keep], dx_grp = dx_grp[keep])
}, by = id]


# Diagnosis periods over time for each id -------------------------------------
person_obs <- obs[, .(obs_min = min(obs_start), obs_max = max(obs_end)), by = id]
dx_best <- dx_best[person_obs, on = "id", nomatch = 0L]
dx_periods <- dx_best[, {
  n <- .N
  start <- dxdate
  end <- if (n > 1L) c(pmin(dxdate[-1L] - 1L, obs_max[1L]), obs_max[1L]) else obs_max[1L]
  end <- pmax(end, start)
  .(dx_start = start, dx_end = end, dx_grp = dx_grp)
}, by = id]


# Add no-dx periods before first dx and for ids with no dx --------------------
first_dx <- dx_periods[, .(first_dx = min(dx_start)), by = id][person_obs, on = "id"]
nodx_pre <- first_dx[obs_min < first_dx,
                     .(id, dx_start = obs_min, dx_end = first_dx - 1L,
                       dx_grp = factor("no-dx", levels = LEVELS))]
ids_dx <- unique(dx_periods$id)
nodx_all <- person_obs[!id %chin% ids_dx,
                       .(id, dx_start = obs_min, dx_end = obs_max,
                         dx_grp = factor("no-dx", levels = LEVELS))]

dx_full <- rbindlist(list(
  dx_periods[, .(id, dx_start, dx_end,
                 dx_grp = factor(as.character(dx_grp), levels = LEVELS))],
  nodx_pre, nodx_all), use.names = TRUE, fill = TRUE)
setorder(dx_full, id, dx_start)


# Intersect with observation windows
setkey(dx_full, id, dx_start, dx_end)
setkey(obs, id, obs_start, obs_end)

ov <- foverlaps(dx_full, obs,
                by.x = c("id", "dx_start", "dx_end"),
                by.y = c("id", "obs_start", "obs_end"),
                type = "any",
                nomatch = 0L
                )

ov[, dx_start := pmax(dx_start, obs_start)]
ov[, dx_end   := pmin(dx_end, obs_end)]

obs_dx_tv <- ov[dx_start <= dx_end,
                .(id, obs_start, obs_end, dx_start, dx_end, dx_grp)]
setorder(obs_dx_tv, id, obs_start, dx_start)

write_fst(obs_dx_tv, file.path(input_dir, "obs_dx_tv.fst"))


# Clean-up
rm(list = ls())
gc()
