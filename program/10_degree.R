## 2 vs 3+ APs

library(fst)
library(data.table)
library(openxlsx)

source("program/00_config.R")
source("program/00_functions.R")

episodes <- read.fst(file.path(input_dir, "episodes.fst"), as.data.table = TRUE)
app60 <- read.fst(file.path(input_dir, "app_periods_60.fst"), as.data.table = TRUE)
app_dx <- read.fst(file.path(input_dir, "app_dx.fst"), as.data.table = TRUE)


# APP episodes overlapping the study window
app60_win <- app60[app_end >= STUDY_START & app_start <= STUDY_END]


# Build ov for add-on analysis
ep <- episodes[, .(id, atc, dup_start, dup_end)]
setkey(ep, id, dup_start, dup_end)
setkey(app60_win, id, app_start, app_end)
ov <- foverlaps(app60_win, ep,
                by.x = c("id", "app_start", "app_end"),
                by.y = c("id", "dup_start", "dup_end"),
                type = "any", nomatch = 0L)
ov[, c_start := as.integer(pmax(dup_start, app_start))]
ov[, c_end   := as.integer(pmin(dup_end, app_end))]

ev <- rbind(
  ov[, .(id, app_start, app_end, t = c_start, d = 1L)],
  ov[, .(id, app_start, app_end, t = c_end, d = -1L)]
)
setorder(ev, id, app_start, app_end, t, d)
ev[, run := cumsum(d), by = .(id, app_start, app_end)]
ev[, t_next := shift(t, type = "lead"), by = .(id, app_start, app_end)]
ev[, seg_len := t_next - t]
ev <- ev[!is.na(seg_len) & seg_len > 0]

# Longest run with concurrency >=3 per episode
ev[, ge3 := run >= 3L]
ev[, blk := cumsum(ge3 != shift(ge3, fill = FALSE)), by = .(id, app_start, app_end)]
blocks <- ev[ge3 == TRUE, .(len = sum(seg_len)), by = .(id, app_start, app_end, blk)]
deg3 <- blocks[, .(max_ge3 = max(len)), by = .(id, app_start, app_end)]

epis <- unique(ov[, .(id, app_start, app_end)])
epis <- merge(epis, deg3, by = c("id", "app_start", "app_end"), all.x = TRUE)
epis[is.na(max_ge3), max_ge3 := 0L]
epis[, three_plus := max_ge3 >= APP_PRIMARY]

# Overall 
per_person <- epis[, .(any3 = any(three_plus)), by = id]
overall <- per_person[, .(n_persons = .N, n_3plus = sum(any3),
                          pct_3plus = round(100 * mean(any3), 1))]

# overall <- epis[, .(N = .N), by = .(level = fifelse(three_plus, "3+", "2"))]
# overall[, pct := round(100 * N / sum(N), 1)]
# setorder(overall, level)
# print(overall)

# # Degree of polypharmacy: 2 vs 3+ APs
# natc <- ov[, .(n_atc = uniqueN(atc)), by = .(id, app_start, app_end)]
# # Overall 2 vs 3+
# overall <- natc[, .(N = .N), by = .(level = fifelse(n_atc >= 3L, "3+", "2"))]
# overall[, pct := 100 * N / sum(N)]

print(overall)
write.xlsx(overall, "output/stable3_degree_app_overall.xlsx")


# By dx_grp
ep_dx <- merge(app_dx[, .(id, app_start, app_end, dx_grp)], 
               epis[, .(id, app_start, app_end, three_plus)],
               by = c("id", "app_start", "app_end"))
pp_dx <- ep_dx[, .(any3 = any(three_plus)), by = .(id, dx_grp)]
by_dx <- pp_dx[, .(n_persons = .N, n_3plus = sum(any3),
                   pct_3plus = round(100 * mean(any3), 1)), by = dx_grp]
by_dx[, dx_grp := factor(dx_grp, levels = LEVELS)]
setorder(by_dx, dx_grp)
print(by_dx)
write.xlsx(by_dx, "output/stable3_degree_app_dx.xlsx")

