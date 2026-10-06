## Numbers for first paragraph in results section

library(fst)
library(data.table)

source("program/00_config.R")
source("program/00_functions.R")
source("input/static/ap_lookup.R")

episodes <- read.fst(file.path(input_dir, "episodes.fst"), as.data.table = TRUE)
app60 <- read.fst(file.path(input_dir, "app_periods_60.fst"), as.data.table = TRUE)
yearly <- read.fst(file.path(input_dir, "yearly.fst"), as.data.table = TRUE)
ap_any <- read.fst(file.path(input_dir, "ap_any.fst"), as.data.table = TRUE)
obs_dx_tv <- read.fst(file.path(input_dir, "obs_dx_tv.fst"), as.data.table = TRUE)


## AP users in study window
ap_years <- unique(
  split_by_year(episodes[, .(id, s = dup_start, e = dup_end)], "s", "e")[
    year %in% STUDY_YEARS, .(id, year)]
  )
n_users <- uniqueN(ap_years$id)

## Person-years of AP treatment
setkey(ap_any, id, ap_start, ap_end)
setkey(obs_dx_tv, id, dx_start, dx_end)
ap_dx <- foverlaps(ap_any, obs_dx_tv,
                   by.x = c("id", "ap_start", "ap_end"),
                   by.y = c("id", "dx_start", "dx_end"),
                   type = "any",
                   nomatch = 0L)

ap_dx[, d := as.integer(pmin(ap_end, dx_end) - pmax(ap_start, dx_start) + 1L)]
total_ap_days <- ap_dx[d > 0, sum(d)]
py_ap <- total_ap_days / 365.25


## Ever-exposed to APP / number of APP episodes
app60_win <- app60[app_end >= STUDY_START & app_start <= STUDY_END]
n_app_users <- uniqueN(app60_win$id) 
pct_ever <- 100 * n_app_users / n_users
n_episodes <- nrow(app60_win)


## Overall APP prevalence 2024
prev_2024 <- yearly[year == 2024, 100 * mean(app)]


## Added AP later start in APP episodes
ep <- episodes[, .(id, atc, dup_start, dup_end)]
setkey(ep, id, dup_start, dup_end)
setkey(app60_win, id, app_start, app_end)
ov <- foverlaps(app60_win, ep,
                by.x = c("id", "app_start", "app_end"),
                by.y = c("id", "dup_start", "dup_end"),
                type = "any", nomatch = 0L)
setorder(ov, id, app_start, app_end, - dup_start)
addon <- ov[, .(atc = atc[1L]), by = .(id, app_start, app_end)]

nm <- function(x) { y <- ap_lookup[. (x), on = .(atc), x.name]
                  fifelse(is.na(y), x, y) }

addon_tab <- addon[, .(N = .N), by = atc][order(-N)]
addon_tab[, pct := 100 * N / sum(N)]
addon_tab[, drug := nm(atc)]

que_addon <- addon[, 100 * mean(atc == QUETIAPINE)]

top5 <- addon_tab[seq_len(min(5L, .N))]


# Write to txt file
lines <- c(
  "Results 3.1 - study population",
  sprintf("AP users %d-%d:     %s",
          STUDY_Y0, STUDY_Y1, format(n_users, big.mark = ",")),
  sprintf("Person-years of AP treatment:     %s",
          format(round(py_ap), big.mark = ",")),
  sprintf("Ever exposed to APP:  %s of %s users (%.1f%%)",
          format(n_app_users, big.mark = ","),
          format(n_users, big.mark = ","), pct_ever),
  sprintf("Overall one-year prevalence 2024: %.1f%%", prev_2024),
  "",
  sprintf("Quetiapine is the add-on in %.1f%% of episodes",
          que_addon),
  "Most frequently added APs:",
  paste0("  ", top5$drug, ": ", sprintf("%.1f%%", top5$pct))
)
writeLines(lines, "output/numbers.txt")
cat(lines, sep = "\n"); cat("\n")
