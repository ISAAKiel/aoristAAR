#' @noRd
make_out_dates <- function(stepstart, stepstop, stepwidth) {
  stepstart <- as.integer(stepstart)
  stepstop  <- as.integer(stepstop)
  stepwidth <- as.integer(stepwidth)

  if (is.na(stepstart) || is.na(stepstop)) {
    return(integer())
  }

  out <- seq(stepstart, stepstop, by = stepwidth)
  out[out <= stepstop]  # defensive: never beyond true stop
}

#' @noRd
make_year_grid <- function(stepstart, stepstop) {
  start_int <- as.integer(stepstart)
  stop_int  <- as.integer(stepstop)
  list(start_int = start_int, stop_int = stop_int, years = start_int:stop_int)
}

#' @noRd
clean_intervals <- function(from, to, start_int, stop_int) {
  f <- as.integer(from)
  t <- as.integer(to)
  ok <- !(is.na(f) | is.na(t))
  f <- f[ok]; t <- t[ok]
  if (length(f) == 0) return(list(f = integer(), t = integer()))

  f <- pmax(f, start_int)
  t <- pmin(t, stop_int)

  ok2 <- f <= t
  list(f = f[ok2], t = t[ok2])
}

#' @noRd
diff_prefix_sum <- function(i_start, i_end, values, n_grid) {
  # values can be integer or numeric
  diff <- rep(0, n_grid + 1L)
  diff[i_start] <- diff[i_start] + values
  endp1 <- i_end + 1L
  in_range <- endp1 <= length(diff)
  diff[endp1[in_range]] <- diff[endp1[in_range]] - values[in_range]
  cumsum(diff)[seq_len(n_grid)]
}

#' @noRd
sample_yearly_to_outdates <- function(yearly, out_dates, start_int) {
  idx <- as.integer(out_dates) - start_int + 1L
  as.numeric(yearly[idx])
}

#' @noRd
interval_indices <- function(f, t, start_int) {
  list(
    i_start = f - start_int + 1L,
    i_end   = t - start_int + 1L
  )
}

#' @noRd
clean_intervals_with_preweights <- function(from, to, start_int, stop_int) {
  f0 <- as.integer(from)
  t0 <- as.integer(to)
  ok <- !(is.na(f0) | is.na(t0))
  f0 <- f0[ok]; t0 <- t0[ok]
  if (length(f0) == 0) return(list(f = integer(), t = integer(), w = numeric()))

  n_years <- abs(f0 - t0)
  n_years[n_years == 0L] <- 1L
  w0 <- 1 / as.numeric(n_years)

  f <- pmax(f0, start_int)
  t <- pmin(t0, stop_int)
  keep <- f <= t

  list(f = f[keep], t = t[keep], w = w0[keep])
}

#' @noRd
empty_ts <- function(out_dates) tibble::tibble(date = out_dates, sum = NA_real_)