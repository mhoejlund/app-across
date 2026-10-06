## Heatmap of APs combined

library(fst)
library(data.table)
library(ggplot2)

source("program/00_config.R")
source("program/00_functions.R")
source("input/static/ap_lookup.R")

TOP_N <- 12L
MIN_LAB <- 1.0

# Full names for panels
dx_labels <- c("F20-29" = "Schizophrenia-spectrum",
               "F30-31" = "Bipolar disorder",
               "F00-03" = "Dementia",
               "F70-79" = "Intellectual disability",
               "F-other" = "Other psychiatric",
               "no-dx" = "No psychiatric diagnosis")


# APs active during APP
mem <- read.fst(file.path(input_dir, "app_membership.fst"), as.data.table = TRUE)
den <- unique(mem[, .(dx_grp, id, seg_start, seg_end)])[, .(n_ep = .N), by = dx_grp]


# Pair-wise counts
pairs <- mem[, {
  a <- sort(unique(atc))
  if (length(a) < 2L) NULL
  else {m <- t(combn(a, 2L)); .(atc1 = m[, 1], atc2 = m[, 2])}
}, by = .(dx_grp, id, seg_start, seg_end)]

pc <- pairs[, .(N = .N), by = .(dx_grp, atc1, atc2)]
pc <- merge(pc, den, by = "dx_grp")
pc[, pct := 100 * N / n_ep]


# Shared drug set and order
nm <- function(x) { y <- ap_lookup[.(x), on = .(atc), x.name]
                  fifelse(is.na(y), x, y)}
freq <- rbind(pc[, .(atc = atc1, pct)], pc[, .(atc = atc2, pct)])[
  , .(w = sum(pct)), by = atc][order(-w)]
keep <- freq[seq_len(min(TOP_N, .N)), atc]
pc <- pc[atc1 %chin% keep & atc2 %chin% keep]
ord <- nm(keep)

pc[, d1 := factor(nm(atc1), levels = ord)]
pc[, d2 := factor(nm(atc2), levels = ord)]
pc[, dx_lab := factor(dx_labels[as.character(dx_grp)],
                      levels = dx_labels[names(dx_labels) %in% dx_grp])]


# One triangle
pc[, r1 := as.integer(d1)]
pc[, r2 := as.integer(d2)]
tri <- pc[, .(dx_lab,
              d1 = factor(ord[pmin(r1, r2)], levels = ord),
              d2 = factor(ord[pmax(r1, r2)], levels = ord),
              N, pct)]

# Label tiles above MIN_LAB
mx <- max(tri$pct)
tri[, lab     := fifelse(pct >= MIN_LAB & N >= 10L, sprintf("%.0f", pct), "")]
tri[, lab_col := fifelse(pct >= 0.55 * mx, "white", "grey15")]
tri[N < 10L, pct := NA_real_]

# # Symmetric matrix
# sym <- rbind(pc[, .(dx_lab, d1, d2, pct)],
#              pc[, .(dx_lab, d1 = d2, d2 = d1, pct)])
# sym[, lab := fifelse(as.integer(d1) < as.integer(d2) & pct >= MIN_LAB,
#                      sprintf("%.0f", pct), "")]


# Plot
p <- ggplot(tri, aes(d1, d2, fill = pct)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = lab, colour = lab_col), size = 2.5) +
  scale_color_identity() +
  facet_wrap(~ dx_lab, ncol = 2) +
  coord_fixed() +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE, limits = rev(ord)) + 
  scale_fill_viridis_c(name = "% of APP episodes", option = "mako",
                       direction = -1, begin = 0.15, limits = c(0, NA)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
    axis.text.y = element_text(colour = "black"),
    strip.text = element_text(face = "bold", hjust = 0),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    legend.key.width = unit(2.4, "lines"),
    legend.key.height = unit(0.6, "lines")
  )

ggsave(file.path(out_dir, "figure3_heatmap_app.png"), p,
       width = 210, height = 265, units = "mm", dpi = 600)
ggsave(file.path(out_dir, "figure3_heatmap_app.pdf"), p,
       width = 210, height = 265, units = "mm", device = cairo_pdf)
