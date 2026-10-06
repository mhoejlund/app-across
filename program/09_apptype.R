## APP type

library(fst)
library(data.table)
library(openxlsx)
library(ggplot2)

source("program/00_config.R")
source("program/00_functions.R")

# Full names for panels
dx_labels <- c("F20-29" = "Schizophrenia-spectrum",
               "F30-31" = "Bipolar disorder",
               "F00-03" = "Dementia",
               "F70-79" = "Intellectual disability",
               "F-other" = "Other psychiatric",
               "no-dx" = "No psychiatric diagnosis")

episodes <- read.fst(file.path(input_dir, "episodes.fst"), as.data.table = TRUE)
app60 <- read.fst(file.path(input_dir, "app_periods_60.fst"), as.data.table = TRUE)
app_dx <- read.fst(file.path(input_dir, "app_dx.fst"), as.data.table = TRUE)

setkey(episodes, id, dup_start, dup_end)
setkey(app60, id, app_start, app_end)

d <- foverlaps(app60, episodes,
               by.x = c("id", "app_start", "app_end"),
               by.y = c("id", "dup_start", "dup_end"),
               type = "any", nomatch = 0L)
d[, is_sed := fifelse(atc == QUETIAPINE,
                      !is.na(avg_dose) & avg_dose < QUET_DOSE_THR,
                      atc %chin% SEDATIVE_AP)]
flags <- d[, .(sedative = any(is_sed),
               clozapine = any(atc == CLOZAPINE),
               pda = any(atc %chin% PDA)),
           by = .(id, app_start, app_end)]


# Attach dx to segments
seg <- merge(app_dx[, .(id, app_start, app_end, dx_grp, seg_start)],
             flags, by = c("id", "app_start", "app_end"))


# Table 3: composition by group (2 periods)
comp <- function(dat, lab) {
  dat[, .(period = lab, N = .N,
          n_sed = sum(sedative, na.rm = TRUE), 
          n_clo = sum(clozapine, na.rm = TRUE), 
          n_pda = sum(pda, na.rm = TRUE)),
      by = dx_grp]
}
t3 <- rbind(comp(seg, "1997-2024"),
            comp(seg[year(seg_start) %in% 2020:2024], "2020-2024"))
t3[, dx_grp := factor(dx_grp, levels = LEVELS)]
setorder(t3, period, dx_grp)

ct <- function (n) fifelse(n < 10L, NA_integer_, as.integer(n))
pc <- function(n, N) fifelse(n < 10L, NA_real_, round(100 * n / N, 1))

t3_out <- t3[, .(
  Period = period,
  `Diagnostic group` = dx_labels[as.character(dx_grp)],
  `APP episodes, N` = N,
  `Sedative, N` = ct(n_sed), `Sedative, %` = pc(n_sed, N),
  `Clozapine, N` = ct(n_clo), `Clozapine, %` = pc(n_clo, N),
  `Partial agonist, N` = ct(n_pda), `Partial agonist, %` = pc(n_pda, N)
)]
t3_out
write.xlsx(t3_out, file.path(out_dir, "table3_app_composition.xlsx"))


# Figure: Sedative-involving APP over time and by group
sed_y <- seg[, .(pct_sed = 100 * mean(sedative)),
             by = .(dx_grp, year = year(seg_start))][year %in% STUDY_YEARS]
sed_y[, dx_lab := dx_labels[as.character(dx_grp)]]

p <- ggplot(sed_y, aes(year, pct_sed, colour = dx_lab)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4, stroke = 0) +
  scale_x_continuous(breaks = seq(STUDY_Y0, STUDY_Y1, 3)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Calendar year", y = "APP episodes involving sedative AP", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "black"),
    legend.position = "bottom"
  )
ggsave(file.path(out_dir, "sfigure_sedative_ap.png"), p,
       width = 190, height = 120, units = "mm", dpi = 600)


# Clean-up
rm(list = ls())
