### Master for APP across psychiatric diagnoses
### MH

# Build dataset
source("program/01_preload.R")
source("program/02_prep.R")
source("program/03_episodes.R")
source("program/04_dxgroups.R")
source("program/05_exposure.R")

# Analysis and output
source("program/06_prevalence.R")
source("program/07_duration.R")
source("program/08_combinations.R")
source("program/09_apptype.R") # Composition of APP 
source("program/10_degree.R") # Text for results paragraph
source("program/11_sensitivity.R") # Text for results paragraph
source("program/12_heatmap.R") # Text for results paragraph
source("program/13_results.R") # Text for results paragraph
source("program/14_scz.R") # Prevalence in SCZ only
source("program/15_sedative.R") # Sedative APs in APP
source("program/16_compare_app_def.R") # APP defined in different ways
