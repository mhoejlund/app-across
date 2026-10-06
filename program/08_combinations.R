## Combinations

library(fst)
library(data.table)
library(openxlsx)

source("program/00_config.R")
source("program/00_functions.R")
source("input/static/ap_lookup.R")

PERIOD <- 2020:2024

# Full names for panels
dx_labels <- c("F20-29" = "Schizophrenia-spectrum",
               "F30-31" = "Bipolar disorder",
               "F00-03" = "Dementia",
               "F70-79" = "Intellectual disability",
               "F-other" = "Other psychiatric",
               "no-dx" = "No psychiatric diagnosis")


# APs active during APP
app_dx <- read.fst(file.path(input_dir, "app_dx.fst"), as.data.table = TRUE)
episodes <- read.fst(file.path(input_dir, "episodes.fst"), as.data.table = TRUE)
app_dx <- app_dx[year(seg_start) %in% PERIOD]

seg <- app_dx[, .(id, dx_grp, seg_start, seg_end)]
ep <- episodes[, .(id, atc, dup_start, dup_end)]
setkey(seg, id, seg_start, seg_end)
setkey(ep, id, dup_start, dup_end)

mem <- foverlaps(seg, ep,
                 by.x = c("id", "seg_start", "seg_end"),
                 by.y = c("id", "dup_start", "dup_end"),
                 type = "any",
                 nomatch = 0L)
mem <- unique(mem[, .(dx_grp, id, seg_start, seg_end, atc)])


# Keep segments with >=2 APs
segk <- mem[, .(k = uniqueN(atc)), by = .(dx_grp, id, seg_start, seg_end)][k >= 2L]
mem <- mem[segk[, .(dx_grp, id, seg_start, seg_end)],
           on = .(dx_grp, id, seg_start, seg_end)]
write.fst(mem, file.path(input_dir, "app_membership.fst"))


# Whole-episode combination per segment
nm <- function(x) { y <- ap_lookup[.(x), on = .(atc), x.name]
                      fifelse(is.na(y), x, y)}
mem[, drug := nm(atc)]
combo <- mem[, .(combo = paste(sort(drug), collapse = " + ")),
             by = .(dx_grp, id, seg_start, seg_end)]


# Overall top 20
tot_ov <- nrow(combo)
ov <- combo[, .(N = .N), by = combo][order(-N)]
ov <- ov[seq_len(min(20L, .N))]
ov[, pct := 100 * N / tot_ov]
ov_out <- ov[, .(Combination = combo,
                 N = fifelse(N < 10L, NA_integer_, N),
                 `%` = fifelse(N < 10L, NA_real_, round(pct, 1)))]
write.xlsx(ov_out, file.path(out_dir, "stable4_app_combinations.xlsx"))


# Top 20 by dx_grp
den <- combo[, .(n_ep = .N), by = dx_grp]
freq <- merge(combo[, .(N = .N), by = .(dx_grp, combo)], den, by = "dx_grp")
freq[, pct := 100 * N / n_ep]
freq[, dx_grp := factor(dx_grp, levels = LEVELS)]
setorder(freq, dx_grp, -N)

fmt <- function(dt) dt[, .(
  `Diagnostic group` = dx_labels[as.character(dx_grp)],
  Combination = combo,
  N = fifelse(N < 10L, NA_integer_, N),
  `%` = fifelse(N < 10L, NA_real_, round(pct, 1))
  )]

top10 <- freq[, head(.SD, 10L), by = dx_grp]
write.xlsx(fmt(top10), file.path(out_dir, "stable4_app_combinations_dx.xlsx"))
write.xlsx(fmt(freq[N >= 10L]), file.path(out_dir, "stable3_app_combinations_dx_full.xlsx"))

print(freq[N < 10L, .(suppressed_combinations = .N,
                      supressed_episodes = sum(N)), by = dx_grp])

# Clean-up
rm(list = ls())
