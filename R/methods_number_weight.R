# simple counting of occurrence
method_number <- function(from, to, stepwidth, stepstart, stepstop) {

  out_dates <- make_out_dates(stepstart, stepstop, stepwidth)
  grid <- make_year_grid(stepstart, stepstop)

  iv <- clean_intervals(from, to, grid$start_int, grid$stop_int)
  f <- iv$f
  t <- iv$t

  if (length(f) == 0) {
    return(tibble::tibble(date = out_dates, sum = NA_real_))
  }

  n_grid <- length(grid$years)

  i_start <- f - grid$start_int + 1L
  i_end   <- t - grid$start_int + 1L

  counts_yearly <- diff_prefix_sum(
    i_start = i_start,
    i_end   = i_end,
    values  = rep.int(1L, length(i_start)),
    n_grid  = n_grid
  )

  idx <- as.integer(out_dates) - grid$start_int + 1L
  out_sum <- as.numeric(counts_yearly[idx])

  out_sum[out_sum == 0] <- NA_real_

  tibble::tibble(date = out_dates, sum = out_sum)
}

# weighting by dating precision
method_weight <- function(from, to, stepwidth, stepstart, stepstop) {

  out_dates <- make_out_dates(stepstart, stepstop, stepwidth)
  grid <- make_year_grid(stepstart, stepstop)

  # weights must be computed BEFORE clamping, to preserve old semantics
  f0 <- as.integer(from)
  t0 <- as.integer(to)
  ok <- !(is.na(f0) | is.na(t0))
  f0 <- f0[ok]; t0 <- t0[ok]

  if (length(f0) == 0) {
    return(tibble::tibble(date = out_dates, sum = NA_real_))
  }

  n_years <- abs(f0 - t0)
  n_years[n_years == 0L] <- 1L
  w0 <- 1 / as.numeric(n_years)

  # now clamp/clean intervals, but keep weights aligned with kept rows
  f <- pmax(f0, grid$start_int)
  t <- pmin(t0, grid$stop_int)
  keep <- f <= t

  f <- f[keep]
  t <- t[keep]
  w <- w0[keep]

  if (length(f) == 0) {
    return(tibble::tibble(date = out_dates, sum = NA_real_))
  }

  n_grid <- length(grid$years)

  i_start <- f - grid$start_int + 1L
  i_end   <- t - grid$start_int + 1L

  weights_yearly <- diff_prefix_sum(
    i_start = i_start,
    i_end   = i_end,
    values  = w,
    n_grid  = n_grid
  )

  idx <- as.integer(out_dates) - grid$start_int + 1L
  out_sum <- weights_yearly[idx]

  out_sum[out_sum == 0] <- NA_real_

  tibble::tibble(date = out_dates, sum = out_sum)
}