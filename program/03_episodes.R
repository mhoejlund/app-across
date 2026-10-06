## Treatment episodes for APP across

library(fst)
library(data.table)
library(readxl)
library(dplyr)
library(PRE2DUPR)

source("program/00_config.R")

rx_ap <- read_fst(file.path(input_dir, "rx_ap.fst"), as.data.table = TRUE)
adm   <- read_fst(file.path(input_dir, "adm.fst"),   as.data.table = TRUE)

# Load parameter files for PRE2DUP
package_parameters <- read_excel(file.path(input_dir, "static/vnrparam_n05a.xlsx"))
atc_parameters     <- get(load(file.path(input_dir, "static/atc_parameters.RData"))) %>% filter(grepl("^N", partial_atc))

# Pass 1: Tune usual duration and dose to cohort
updated_params <- pre2dup(
  pre_data = rx_ap, 
  pre_person_id = "id",
  pre_atc = "atc",
  pre_package_id = "drug_id",
  pre_date = "rxdate",
  pre_ratio = "n_packs", # Number of packages
  pre_ddd = "ddd",
  package_parameters = package_parameters,
  pack_atc = "ATC",
  pack_id = "VNR",
  pack_ddd_low = "lower_ddd",
  pack_ddd_usual = "usual_ddd",
  pack_dur_min = "minimum_dur",
  pack_dur_usual = "usual_dur",
  pack_dur_max = "maximum_dur",
  atc_parameters = atc_parameters,
  atc_class = "partial_atc",
  atc_ddd_low = "lower_ddd_atc",
  atc_ddd_usual = "usual_ddd_atc",
  atc_dur_min = "minimum_dur_atc",
  atc_dur_max = "maximum_dur_atc",
  hosp_data = adm,
  hosp_person_id = "id",
  hosp_admission = "enc_start",
  hosp_discharge = "enc_end",
  date_range = c(DATA_START, DATA_END),
  days_covered = DAYS_COVERED,
  data_to_return = "parameters",
  drop_atcs = TRUE,
  post_process_perc = 1
)

# Update usual duration
updated_params$usual_dur_new <- ifelse(!is.na(updated_params$common_duration),
                                       updated_params$common_duration,
                                       updated_params$usual_dur)

# Update usual DDD/day
updated_params$usual_ddd_new <- updated_params$DDDpack/updated_params$usual_dur_new
updated_params$lower_ddd     <- ifelse(updated_params$usual_ddd_new < updated_params$lower_ddd,
                                       updated_params$usual_ddd_new,
                                       updated_params$lower_ddd)

# Pass 2: Create periods with updated parameters
episodes <- pre2dup(
  pre_data = rx_ap,
  pre_person_id = "id",
  pre_atc = "atc",
  pre_package_id = "drug_id",
  pre_date = "rxdate",
  pre_ratio = "n_packs", # Number of packages
  pre_ddd = "ddd",
  package_parameters = updated_params,
  pack_atc = "ATC",
  pack_id = "VNR",
  pack_ddd_low = "lower_ddd",
  pack_ddd_usual = "usual_ddd_new",
  pack_dur_min = "minimum_dur",
  pack_dur_usual = "usual_dur_new",
  pack_dur_max = "maximum_dur",
  atc_parameters = atc_parameters,
  atc_class = "partial_atc",
  atc_ddd_low = "lower_ddd_atc",
  atc_ddd_usual = "usual_ddd_atc",
  atc_dur_min = "minimum_dur_atc",
  atc_dur_max = "maximum_dur_atc",
  hosp_data = adm,
  hosp_person_id = "id",
  hosp_admission = "enc_start",
  hosp_discharge = "enc_end",
  date_range = c(DATA_START, DATA_END),
  days_covered = DAYS_COVERED,
  data_to_return = "periods",
  drop_atcs = TRUE,
  post_process_perc = 1
)

episodes <- episodes[, .(id = as.numeric(as.character(id)),
                         atc, dup_start, dup_end, 
                         avg_dose = dup_temporal_average_DDDs)]
write.fst(episodes, file.path(input_dir, "episodes.fst"))


# Clean-up
rm(list = ls())
gc()
