## Schizophrenia only

library(fst)
library(data.table)
library(openxlsx)

source("program/00_config.R")
source("program/00_functions.R")

app60 <- read.fst(file.path(input_dir, "app_periods_60.fst"), as.data.table = TRUE)
episodes <- read.fst(file.path(input_dir, "episodes.fst"), as.data.table = TRUE)
dx <- read.fst(file.path(input_dir, "dx.fst"), as.data.table = TRUE)
dx[, dxdate := as.Date(dxdate)]


# Building blocks
ap_years <- unique(
  split_by_year(episodes[, .(id, s = dup_start, e = dup_end)],
                "s", "e")[year %in% STUDY_YEARS, .(id, year)]
)

app_years <- unique(
  split_by_year(app60[, .(id, s = app_start, e = app_end)],
                "s", "e")[year %in% STUDY_YEARS, .(id, year, app = TRUE)]
)


# Cohort restrictions
f20 <- dx[grepl("^295|^F20", diag), .(f20_date = min(dxdate)), by = id]
forensic_ids <- unique(dx[grepl("^Z046[12]", gsub("\\.", "", diag)), id])
scz <- ap_years[f20, on = "id", nomatch = 0L]
scz[, yr_since := year - year(f20_date)]
scz <- scz[yr_since >= 0L]

# Eligible person-years at each step
elig1 <- scz[, .(id, year)]
elig2 <- scz[!id %in% forensic_ids, .(id, year)]
elig3 <- scz[!id %in% forensic_ids & yr_since >= 2, .(id, year)]

# One-year prevalence for each set
prev_step <- function(elig) {
  d <- merge(elig, app_years, by = c("id", "year"), all.x = TRUE)
  d[is.na(app), app:= FALSE]
  d[, .(n = .N, n_app = sum(app), prev = 100 * mean(app)), by = year][order(year)]
}

bench <- rbindlist(list(
  prev_step(elig1)[, step := "1_F20"],
  prev_step(elig2)[, step := "2_non_forensic"],
  prev_step(elig3)[, step := "3_washout_2y"]
))

print(dcast(bench[year == 2024], year ~ step, value.var = "prev"))

# Ever-APP within each step's eligible years
ever <- function(elig) {
  d <- merge(elig, app_years, by = c("id", "year"), all.x = TRUE)
  100 * uniqueN(d[app == TRUE, id]) / uniqueN(d$id)
}

cat(sprintf("Ever-APP step1 %.1f step2 %.1f step3 %.1f\n",
            ever(elig1), ever(elig2), ever(elig3)))

# Reference F20-29
yearly <- read.fst("input/yearly.fst", as.data.table = TRUE)
print(yearly[dx_grp == "F20-29", .(prev = 100 * mean(app)), by = year][year == 2024])

write.xlsx(dcast(bench, year ~ step, value.var = "prev"),
           "output/stable_benchmark.xlsx")

## Extra check
app_state <- read.fst(file.path(input_dir, "app_periods_state_60.fst"), as.data.table = T)
app_years_state <- unique(
  split_by_year(app_state[, .(id, s = app_start, e = app_end)], "s", "e")[
    year %in% STUDY_YEARS, .(id, year, app = TRUE)
  ]
)

prev_state <- function(elig) {
  d <- merge(elig, app_years_state, by = c("id", "year"), all.x = TRUE)
  d[is.na(app), app := FALSE]
  d[, .(prev = 100 * mean(app)), by = year][order(year)]
}

prev_state(elig3)[year == 2024]
