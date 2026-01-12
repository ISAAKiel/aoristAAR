#' @noRd
method_number <- function(from, to, stepwidth, stepstart, stepstop, interval = "[]") {
  out_dates <- make_out_dates(stepstart, stepstop, stepwidth)
  grid <- make_year_grid(stepstart, stepstop)

  iv <- clean_intervals(from, to, grid$start_int, grid$stop_int)
  f <- iv$f
  t <- iv$t

  clo <- apply_interval_closure(f, t, interval = interval)
  f <- clo$f
  t <- clo$t

  if (length(f) == 0) return(empty_ts(out_dates))

  n_grid <- grid$stop_int - grid$start_int + 1L
  ii <- interval_indices(f, t, grid$start_int)

  counts_yearly <- diff_prefix_sum(
    i_start = ii$i_start,
    i_end   = ii$i_end,
    values  = rep.int(1L, length(ii$i_start)),
    n_grid  = n_grid
  )

  out_sum <- sample_yearly_to_outdates(counts_yearly, out_dates, grid$start_int)
  out_sum <- zero_to_na(out_sum)

  tibble::tibble(date = out_dates, sum = out_sum)
}

#' @noRd
method_weight <- function(from, to, stepwidth, stepstart, stepstop, interval = "[]") {
  out_dates <- make_out_dates(stepstart, stepstop, stepwidth)
  grid <- make_year_grid(stepstart, stepstop)

  iv <- clean_intervals_with_preweights(from, to, grid$start_int, grid$stop_int)
  f <- iv$f
  t <- iv$t
  w <- iv$w

  clo <- apply_interval_closure(f, t, interval = interval)
  f <- clo$f
  t <- clo$t
  if (!is.null(clo$keep)) w <- w[clo$keep]

  if (length(f) == 0) return(empty_ts(out_dates))

  n_grid <- grid$stop_int - grid$start_int + 1L
  ii <- interval_indices(f, t, grid$start_int)

  weights_yearly <- diff_prefix_sum(
    i_start = ii$i_start,
    i_end   = ii$i_end,
    values  = w,
    n_grid  = n_grid
  )

  out_sum <- sample_yearly_to_outdates(weights_yearly, out_dates, grid$start_int)
  out_sum <- zero_to_na(out_sum)

  tibble::tibble(date = out_dates, sum = out_sum)
}