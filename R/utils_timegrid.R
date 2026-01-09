#' @noRd
make_out_dates <- function(stepstart, stepstop, stepwidth) {
  seq(stepstart, stepstop, by = stepwidth)
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