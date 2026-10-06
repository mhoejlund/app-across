## Sensitivity analysis on duration thresholds

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
               "no-dx" = "No recorded psychiatric diagnosis",
               "Overall" = "Overall")

episodes <- read.fst(file.path(input_dir, "episodes.fst"), as.data.table = TRUE)
obs_dx_tv <- read.fst(file.path(input_dir, "obs_dx_tv.fst"), as.data.table = TRUE)

# Denominator
ap_years <- unique(
  split_by_year(episodes[, .(id, s = dup_start, e = dup_end)],
                "s", "e")[year %in% STUDY_YEARS, .(id, year)]
)

dx_years <- split_by_year(
  obs_dx_tv[, .(id, s = dx_start, e = dx_end, dx_grp)], "s", "e"
)[year %in% STUDY_YEARS]
dx_years[, dx := factor(dx_grp, levels = LEVELS)]
dx_year <- dx_years[, .(dx_grp = dx_grp[which.min(as.integer(dx_grp))]),
                    by = .(id, year)]


# Prevalence under each threshold
sens <- rbindlist(lapply(APP_THRESHOLDS, function(thr) {
  app <- read.fst(sprintf("input/app_periods_%d.fst", thr),
                  as.data.table = TRUE)
  prev_yr(make_yearly(app, ap_years, dx_year, LEVELS))[, threshold := thr][]
}))


# Suppress if N<10
sens[, cell := fifelse(n_app < 10L, "N<10", sprintf("%.1f", prev))]

 
# Label for group column
sens[, dx_lab := dx_labels[as.character(dx_grp)]]
sens[is.na(dx_lab), dx_lab := as.character(dx_grp)]
grp_levels <- c(unname(dx_labels), "Overall")
grp_levels <- grp_levels[!duplicated(grp_levels)]
sens[, dx_lab := factor(dx_lab, levels = grp_levels)]

thr_cols <- paste0(APP_THRESHOLDS, " days")


# Prevalence 2024 by dx_grp
tab24 <- dcast(sens[year == 2024], dx_lab ~ threshold, value.var = "cell")
setnames(tab24, as.character(APP_THRESHOLDS), thr_cols)
setnames(tab24, "dx_lab", "Diagnostic group")
setorder(tab24, `Diagnostic group`)
print(tab24)
write.xlsx(tab24, "output/stable5_sensitivity_thresholds_2024.xlsx")

# Output full series
tabfull <- dcast(sens, year + dx_lab ~ threshold, value.var = "cell")
setnames(tabfull, as.character(APP_THRESHOLDS), thr_cols)
setnames(tabfull, c("year", "dx_lab"), c("Year", "Diagnostic group"))
setorder(tabfull, `Diagnostic group`, Year)
print(tabfull)
write.xlsx(tabfull, "output/stable5_sensitivity_thresholds_allyears.xlsx")

# Figure
plt <- copy(sens)
plt[, prev_plot := fifelse(n_app < 10L, NA_real_, prev)]
plt[, thr_f := factor(threshold, levels = APP_THRESHOLDS,
                      labels = paste0(APP_THRESHOLDS, " days"))]

pfig <- ggplot(plt, aes(year, prev_plot, colour = thr_f)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 0.8, stroke = 0, na.rm = TRUE) + 
  facet_wrap(~ dx_lab, ncol = 2, scales = "free_y") + 
  scale_x_continuous(breaks = seq(STUDY_Y0, STUDY_Y1, 6)) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_colour_viridis_d(option = "mako", end = 0.85, direction = -1,
                         name = "APP definition") +
  labs(x = "Calendar year", y = "One-year prevalence of APP (%)") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "black"),
    strip.text = element_text(face = "bold", hjust = 0),
    legend.position = "bottom"
  )

ggsave("output/sfigure1_threshold.png", pfig,
       width = 200, height = 235, units = "mm", dpi = 600)
