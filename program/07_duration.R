## Duration

library(fst)
library(data.table)
library(openxlsx)

source("program/00_config.R")
source("program/00_functions.R")


dx_labels <- c("F20-29" = "Schizophrenia-spectrum",
               "F30-31" = "Bipolar disorder",
               "F00-03" = "Dementia",
               "F70-79" = "Intellectual disability",
               "F-other" = "Other psychiatric disorders",
               "no-dx" = "No recorded psychiatric diagnosis")


app_dx <- read.fst(file.path(input_dir, "app_dx.fst"), as.data.table = TRUE)


# By dx-grp
dur <- app_dx[, .(
  n   = .N,
  med = median(seg_days / MONTH_DAYS),
  q1  = quantile(seg_days / MONTH_DAYS, 0.25),
  q3  = quantile(seg_days / MONTH_DAYS, 0.75),
  p6  = 100 * mean(seg_days >= 6 * MONTH_DAYS),
  p12 = 100 * mean(seg_days >= 12 * MONTH_DAYS),
  p24 = 100 * mean(seg_days >= 24 * MONTH_DAYS)
), by = dx_grp]
dur[, dx_grp := factor(dx_grp, levels = LEVELS)]
setorder(dur, dx_grp)

fmt <- function(d) d[, .(
  `Diagnostic group`     = grp,
  `Episodes, N`          = n,
  `Median months (IQR)`  = sprintf("%.1f (%.1f-%.1f)", med, q1, q3),
  `>=6 months, %`        = sprintf("%.1f", p6),
  `>=12 months, %`       = sprintf("%.1f", p12),
  `>=24 months, %`       = sprintf("%.1f", p24)
)]
dur[, grp := dx_labels[as.character(dx_grp)]]
tab <- fmt(dur)


# Overall
app60 <- read.fst(file.path(input_dir, "app_periods_60.fst"), as.data.table = TRUE)
app_w <- app60[app_end >= STUDY_START & app_start <= STUDY_END]
app_w[, dm := as.integer(pmin(app_end, STUDY_END) -
                           pmax(app_start, STUDY_START) + 1L) / MONTH_DAYS]

overall <- data.table(
  `Diagnostic group`     = "Overall",
  `Episodes, N`          = nrow(app_w),
  `Median months (IQR)`  = sprintf("%.1f (%.1f-%.1f)",
                                   median(app_w$dm),
                                   quantile(app_w$dm, 0.25),
                                   quantile(app_w$dm, 0.75)),
  `>=6 months, %`        = sprintf("%.1f", 100 * mean(app_w$dm >= 6)),
  `>=12 months, %`       = sprintf("%.1f", 100 * mean(app_w$dm >= 12)),
  `>=24 months, %`       = sprintf("%.1f", 100 * mean(app_w$dm >= 24))  
)
tab <- rbind(tab, overall)
write.xlsx(tab, file.path(out_dir, "table2_app_duration.xlsx"))


# Clean-up
rm(list = ls())
