## Functions for APP project

## Construct APP episodes
make_app_periods <- function(episodes, min_days = 60L) {
  eps <- as.data.table(episodes)
  eps <- eps[dup_end >= dup_start]
  eps1 <- eps[, .(id, atc1 = atc, s1 = dup_start, e1 = dup_end)]
  eps2 <- eps[, .(id, atc2 = atc, s2 = dup_start, e2 = dup_end)]
  setkey(eps1, id, s1, e1); setkey(eps2, id, s2, e2)
  
  ov <- foverlaps(eps1, eps2,
                  by.x = c("id", "s1", "e1"),
                  by.y = c("id", "s2", "e2"),
                  type = "any",
                  nomatch = 0L
  )
  
  # Keep only true APP
  ov <- ov[atc1 < atc2]
  ov[, `:=` (ov_start = pmax(s1, s2),
             ov_end   = pmin(e1, e2))]
  ov <- ov[ov_start <= ov_end, .(id, atc1, atc2, ov_start, ov_end)]
  
  # Union of intervals per id
  setorder(ov, id, atc1, atc2, ov_start, ov_end)
  ov[, grp := cumsum(c(TRUE, as.integer(ov_start[-1L]) > cummax(as.integer(ov_end[-.N])) + 1L)),
     by = .(id, atc1, atc2)]
  
  pair <- ov[, .(app_start = min(ov_start), app_end = max(ov_end)),
             by = .(id, atc1, atc2, grp)]
  pair[, duration := as.integer(app_end - app_start + 1L)]
  pair[duration >= min_days, .(id, atc1, atc2, app_start, app_end, duration)]
}



make_app_periods_anyoverlap <- function(episodes, min_days = 60L) {
  eps <- as.data.table(episodes)
  eps <- eps[dup_end >= dup_start]
  eps1 <- eps[, .(id, atc1 = atc, s1 = dup_start, e1 = dup_end)]
  eps2 <- eps[, .(id, atc2 = atc, s2 = dup_start, e2 = dup_end)]
  setkey(eps1, id, s1, e1); setkey(eps2, id, s2, e2)

  ov <- foverlaps(eps1, eps2,
                  by.x = c("id", "s1", "e1"),
                  by.y = c("id", "s2", "e2"),
                  type = "any",
                  nomatch = 0L
  )

  # Keep only true APP
  ov <- ov[atc1 < atc2]
  ov[, `:=` (app_start = pmax(s1, s2),
             app_end   = pmin(e1, e2))]
  ov <- ov[app_start <= app_end, .(id, app_start, app_end)]

  # Union of intervals per id
  setorder(ov, id, app_start, app_end)
  ov[, grp := cumsum(c(TRUE, as.integer(app_start[-1L]) > cummax(as.integer(app_end[-.N])) + 1L)),
     by = .(id)]
  

  app <- ov[, .(app_start = min(app_start),
                app_end = max(app_end)), by = .(id, grp)]

  app[, duration := as.integer(app_end - app_start + 1L)]
  app[duration >= min_days, .(id, app_start, app_end, duration)]
}



# Merge overlapping/adjacent intervals per id
union_intervals <- function(dt, id_col, start_col, end_col) {
  d <- data.table(
    id = dt[[id_col]],
    s  = as.integer(as.Date(dt[[start_col]])),
    e  = as.integer(as.Date(dt[[end_col]]))
  )
  
  d <- d[e >= s]
  setorder(d, id, s, e)
  d[, grp := cumsum(c(TRUE, s[-1L] > cummax(e[-.N]) + 1L)), by = id_col]
  out <- d[, .(ap_start = as.Date(min(s), origin = "1970-01-01"),
               ap_end   = as.Date(max(e), origin = "1970-01-01")),
            by = .(id, grp)]
  out[, grp := NULL]
}



# Person-level APP time
to_person <- function(pairs) {
  d <- pairs[, .(id, s = as.integer(app_start), e = as.integer(app_end))]
  setorder(d, id, s, e)
  d[, grp := cumsum(c(TRUE, s[-1L] > cummax(e[-.N]) + 1L)), by = id]
  out <- d[, .(app_start = as.Date(min(s), origin = "1970-01-01"),
               app_end   = as.Date(max(e), origin = "1970-01-01")),
           by = .(id, grp)]
  out[, grp := NULL]
  out[, duration := as.integer(app_end - app_start + 1L)][]
}



# Split intervals into calendar-year pieces
split_by_year <- function(dt, start_col, end_col) {
  dt <- as.data.table(dt)
  dt[, {
    s <- .SD[[start_col]]
    e <- .SD[[end_col]]
    yrs <- year(s):year(e)
    data.table(
      year = yrs,
      ys = pmax(s, as.Date(paste0(yrs, "-01-01"))),
      ye = pmin(e, as.Date(paste0(yrs, "-12-31")))
    )
  }, by = setdiff(names(dt), c(start_col, end_col)), 
  .SDcols = c(start_col, end_col)]
}



# APP days by year x dx-grp
calc_app_days <- function(dx_y, app_x) {
  dx_x <- dx_y[, .(id, year, dx_grp, ys, ye)]
  dx_x <- dx_x[ys <= ye]; app_x <- app_x[ys <= ye]
  setkey(dx_x, id, year, ys, ye)
  setkey(app_x, id, year, ys, ye)
  
  ov <- foverlaps(dx_x, app_x,
                  by.x = c("id", "year", "ys", "ye"),
                  by.y = c("id", "year", "ys", "ye"),
                  type = "any",
                  nomatch = 0L
  )
  ov[, seg_start := pmax(i.ys, ys)]
  ov[, seg_end   := pmin(i.ye, ye)]
  ov <- ov[seg_start <= seg_end]
  ov[, app_days := as.integer(seg_end - seg_start + 1L)]
  ov[, .(app_days = sum(app_days)), by = .(year, dx_grp)]
}



# Person-year APP
make_yearly <- function(app, ap_years, dx_year, levels) {
  app_years <- unique(
    split_by_year(app[, .(id, s = app_start, e = app_end)],
                  "s", "e")[, .(id, year, app = TRUE)]
  )
  y <- merge(ap_years, app_years, by = c("id", "year"), all.x = TRUE)
  y[is.na(app), app := FALSE]
  y <- merge(y, dx_year, by = c("id", "year"), all.x = TRUE)
  y[is.na(dx_grp), dx_grp := factor("no-dx", levels = levels)]
  y[]
}



# One-year prevalence by dx_grp + overall row
prev_yr <- function(y) {
  by_dx <- y[, .(n_ap = .N, n_app = sum(app), prev = 100 * mean(app)),
             by = .(year, dx_grp = as.character(dx_grp))]
  overall <- y[, .(dx_grp = "Overall", n_ap = .N, n_app = sum(app),
                   prev = 100 * mean(app)),
               by = year]
  rbind(by_dx, overall)[order(dx_grp, year)]
}
