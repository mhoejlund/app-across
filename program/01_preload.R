## Sampling/preload for APP across

library(fst)
library(data.table)

source("program/00_config.R")

set.seed(SEED)

# Load files
rx      <- read_fst(file.path(raw_dir, "drug.fst"),        as.data.table = TRUE)
dx      <- read_fst(file.path(raw_dir, "diagnosis.fst"),   as.data.table = TRUE)
enc     <- read_fst(file.path(raw_dir, "encounter.fst"),   as.data.table = TRUE)
obs     <- read_fst(file.path(raw_dir, "observation.fst"), as.data.table = TRUE)
person  <- read_fst(file.path(raw_dir, "person.fst"),      as.data.table = TRUE)

# Restric cohort to AP users in study window and sample if SAMPLEFRAC < 1
ap_users <- unique(rx[grepl(AP_PATTERN, atc) &
                        rxdate %between% c(DATA_START, DATA_END), .(id)])
ap_users <- ap_users[, .(id = sample(id, round(SAMPLEFRAC * .N)))]
setkey(ap_users, id)

# Sample from source files to input folder
setkey(rx, id)
write.fst(rx[ap_users, .(id, rxdate, atc, ddd, drug_id), nomatch = 0],
          file.path(input_dir, "rx.fst"))

setkey(dx, id)
write.fst(dx[ap_users, .(id, enc_id, diag, classification, dxparent, dxdate, dxtype), nomatch = 0],
          file.path(input_dir, "dx.fst"))

setkey(enc, id)
write.fst(enc[ap_users, .(id, enc_id, enc_start, enc_end, enctype, org_id), nomatch = 0],
          file.path(input_dir, "enc.fst"))

setkey(obs, id)
write.fst(obs[ap_users, .(id, obs_start, obs_end, end_reason), nomatch = 0],
          file.path(input_dir, "obs.fst"))

setkey(person, id)
write.fst(person[ap_users, .(id, sex, birthdate, deathdate), nomatch = 0],
          file.path(input_dir, "person.fst"))


# Clean-up
rm(list = ls())
gc()
