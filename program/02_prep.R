## Prescription file clean-up for APP across

library(fst)
library(data.table)

source("program/00_config.R")

# Select AP precsriptions within study window
rx <- read_fst(file.path(input_dir, "rx.fst"), as.data.table = TRUE)
rx_ap <- rx[grepl(AP_PATTERN, atc) & !atc %chin% AP_EXCLUDE]
rx_ap <- rx_ap[rxdate >= DATA_START & rxdate <= DATA_END]
rx_ap[, drug_id := as.numeric(drug_id)]
setkey(rx_ap, drug_id)

# Add ddd_pack from drug_info
druginfo <- read_fst(file.path(raw_dir, "drug_info.fst"), as.data.table = TRUE)
druginfo <- druginfo[grepl(AP_PATTERN, atc),
                     .(drug_id = as.numeric(drug_id),
                       ddd_pack = as.numeric(ddd),
                       strength = strength,
                       amount = package_amount,
                       adm_route = adm_route)]
setkey(druginfo, drug_id)

rx_ap <- druginfo[rx_ap, on = .(drug_id)]
rx_ap[, n_packs := round(ddd / ddd_pack)]
rx_ap[, stk := round(n_packs * amount)]

# Mark LAI prescriptions 
rx_ap[LAI_LOOKUP, lai_str_min := i.lai_str_min, on = "atc"]
rx_ap[, is_lai := !is.na(lai_str_min) & strength >= lai_str_min]

# Drop dispensings <7 tables (but keep LAIs)
rx_ap <- rx_ap[stk >= STK_MIN | is_lai]
rx_ap[, lai_str_min := NULL]
write.fst(rx_ap, file.path(input_dir, "rx_ap.fst"))


# Admissions for PRE2DUP
enc <- read_fst(file.path(input_dir, "enc.fst"), as.data.table = TRUE)
adm <- enc[enctype == 1, .(id,
                           enc_start = as.Date(enc_start), 
                           enc_end = as.Date(enc_end))]
adm <- adm[(enc_end - enc_start) > 2]
setorder(adm, id, enc_start)
adm[, grp := cumsum(c(TRUE, as.numeric(enc_start)[-1L] >
                        cummax(as.numeric(enc_end)[-.N]))), by = id]
adm <- adm[, .(enc_start = min(enc_start), enc_end = max(enc_end)),
           by = .(id, grp)][, grp := NULL]
write.fst(adm, file.path(input_dir, "adm.fst"))


# Clean-up
rm(list = ls())
gc()
