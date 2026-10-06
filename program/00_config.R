## Configurations for APP across

# Paths
raw_dir   <- "RAW/DIRECTORY/FOLDER"
input_dir <- "input"
out_dir   <- "output"

# Study window
DATA_START  <- as.Date("1995-01-01")
DATA_END    <- as.Date("2025-06-30") 
STUDY_START <- as.Date("1997-01-01")
STUDY_END   <- as.Date("2024-12-31")
FU_END      <- as.Date("2024-12-31")
DIAG_START  <- as.Date("1960-01-01")
STUDY_Y0    <- 1997
STUDY_Y1    <- 2024
STUDY_YEARS <- STUDY_Y0:STUDY_Y1

# Cohort building
SAMPLEFRAC  <- 1
SEED        <- 20260101

# AP definition
AP_PATTERN  <- "^N05A[A-LX]"
AP_EXCLUDE  <- c("N05AA04",  # acepromazine
                 "N05AB01",  # dixyrazine
                 "N05AB04",  # prochlorperazine
                 "N05AD06",  # bromperidol
                 "N05AD08",  # droperidol
                 "N05AN01")  # lithium
QUETIAPINE  <- "N05AH04"
QUET_DOSE_THR <- 0.25
SEDATIVE_AP <- c("N05AA02",  # levomepromazine
                 "N05AD03",  # melperone
                 "N05AF03")  # chlorprothixene
CLOZAPINE   <- "N05AH02"
PDA         <- c("N05AX12",   # aripiprazole
                 "N05AX15",   # cariprazine
                 "N05AX16")   # brexpiprazole

LAI_LOOKUP <- data.table(
  atc      = c("N05AB02",
               "N05AB03",
               "N05AD01",
               "N05AF01",
               "N05AF05",
               "N05AH03",
               "N05AX08",
               "N05AX12", 
               "N05AX13"
               ),
  lai_str_min = c(25,
               50,
               50,
               20,
               200,
               210,
               25,
               300,
               50
               )
)

# APP definition
APP_PRIMARY <- 60L
APP_THRESHOLDS <- c(30L, 60L, 90L, 120L)

# Diagnostic hierachy
PRIO <- c("F20-29", "F30-31", "F00-03", "F70-79", "F-other")
LEVELS <- c(PRIO, "no-dx")

# Units
MONTH_DAYS <- 365.25 / 12

# PRE2DUP parameters
DAYS_COVERED <- 14L
STK_MIN <- 7L

# DDD fallback
WHO_DDD <- data.table::data.table(
  atc = c("N05AB02", "N05AB03", "N05AD03"),
  ddd_mg = c(NA_real_, NA_real_, NA_real_)
)


