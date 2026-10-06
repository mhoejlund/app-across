## Prevalence

library(fst)
library(data.table)
library(openxlsx)
library(ggplot2)
library(scales)

source("program/00_config.R")
source("program/00_functions.R")


dx_labels <- c("F20-29" = "Schizophrenia-spectrum",
               "F30-31" = "Bipolar disorder",
               "F00-03" = "Dementia",
               "F70-79" = "Intellectual disability",
               "F-other" = "Other psychiatric disorders",
               "no-dx" = "No recorded psychiatric diagnosis",
               "Overall" = "Overall")


# Prevalence by year and group + overall
yearly <- read.fst(file.path(input_dir, "yearly.fst"), as.data.table = TRUE)

prev_dx <- yearly[, .(n_ap = .N, n_app = sum(app), prev = 100 * mean(app)),
                  by = .(year, dx_grp = as.character(dx_grp))]
prev_ov <- yearly[, .(dx_grp = "Overall", n_ap = .N, n_app = sum(app),
                      prev = 100 * mean(app)), by = year]
prev <- rbind(prev_dx, prev_ov)[year %in% STUDY_YEARS]
prev[, dx_grp := factor(dx_grp, levels = names(dx_labels))]
setorder(prev, dx_grp, year)


# Figure 2: Prevalence by year
prev[, dx_lab := dx_labels[as.character(dx_grp)]]
lab_dt <- prev[!is.na(prev), .SD[which.max(year)], by = dx_grp]
lab_dt[, x_lab := STUDY_Y1 + 0.4]

p <- ggplot(prev, aes(year, prev, colour = dx_lab)) +
  geom_line(aes(linetype = dx_lab == "Overall"), linewidth = 0.9) +
  geom_point(size = 1.4, stroke = 0) +
  geom_text(data = lab_dt, aes(x = x_lab, y = prev, label = dx_lab),
            hjust = 0, size = 2.9, fontface = "bold", show.legend = FALSE) +
  scale_x_continuous(breaks = seq(STUDY_Y0, STUDY_Y1, 3),
                     expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5),
                     labels = label_percent(scale = 1)) +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "22"),
                        guide = "none") +
  coord_cartesian(xlim = c(STUDY_Y0, STUDY_Y1 + 8), clip = "off") +
  labs(x = "\nCalendar year", y = "One-year prevalence of APP\n") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(colour = "black"),
    legend.position = "none",
    plot.margin = margin(6, 90, 6, 6)
  )
ggsave(file.path(out_dir, "figure2_app_year_dx.png"), p,
       width = 190, height = 120, units = "mm", dpi = 600)
ggsave(file.path(out_dir, "figure2_app_year_dx.pdf"), p,
       width = 190, height = 120, units = "mm", device = cairo_pdf)


# Table S2: Data for figure 2
st2 <- copy(prev)
st2[, cell := fifelse(n_app < 10, "<10", sprintf("%d (%.1f%%)", n_app, prev))]
st2_wide <- dcast(st2, year ~ dx_grp, value.var = "cell")
write.xlsx(st2_wide, file.path(out_dir, "stable2_prev_year_dx.xlsx"))


# Table 1: AP and APP exposure (days) by group
ap_any <- read.fst(file.path(input_dir, "ap_any.fst"), as.data.table = TRUE)
obs_dx_tv <- read.fst(file.path(input_dir, "obs_dx_tv.fst"), as.data.table = TRUE)
setkey(ap_any, id, ap_start, ap_end)
setkey(obs_dx_tv, id, dx_start, dx_end)

ap_dx <- foverlaps(ap_any, obs_dx_tv,
                   by.x = c("id", "ap_start", "ap_end"),
                   by.y = c("id", "dx_start", "dx_end"),
                   type = "any",
                   nomatch = 0L)


# AP days per dx
ap_dx[, ap_days := as.integer(pmin(ap_end, dx_end, STUDY_END) -
                                pmax(ap_start, dx_start, STUDY_START) + 1L)] # Added clipping to study period
ap_days_dx <- ap_dx[ap_days > 0, .(ap_days = sum(ap_days)), by = dx_grp]


# APP days per dx
app_dx <- read.fst(file.path(input_dir, "app_dx.fst"), as.data.table = TRUE)
app_days_dx <- app_dx[seg_days > 0, .(app_days = sum(seg_days)), by = dx_grp]


t1 <- merge(ap_days_dx, app_days_dx, by = "dx_grp", all.x = TRUE)
t1[is.na(app_days), app_days := 0]
t1 <- rbind(t1, t1[, .(dx_grp = "Overall",
                       ap_days = sum(ap_days), app_days = sum(app_days))])
t1[, pct_app_of_ap := 100 * app_days / ap_days]
t1[, pct_app_of_all_app := 100 * app_days / t1[dx_grp == "Overall", app_days]]
t1[, dx_grp := factor(dx_grp, levels = names(dx_labels))]
setorder(t1, dx_grp)

t1_out <- t1[, .(
  `Diagnostic group` = dx_labels[as.character(dx_grp)],
  `AP days`  = formatC(ap_days, format = "d", big.mark = ","),
  `APP days` = formatC(app_days, format = "d", big.mark = ","),
  `APP of AP, %` = sprintf("%.1f", pct_app_of_ap),
  `Of all APP days, %` = sprintf("%.1f", pct_app_of_all_app)
)]
write.xlsx(t1_out, file.path(out_dir, "table1_app_by_dx.xlsx"))


# Clean-up
rm(list = ls())
