# weighting by period attribution
#' @noRd
method_period_correction <- function(from, to, stepwidth, stepstart, stepstop, correct = TRUE) {

  if (stepwidth != 1) {
    stop("Method 'period_correction' only works with stepwidth = 1.", call. = FALSE)
  }

  # align with seq2ts guard
  if (all(is.na(from)) || all(is.na(to))) {
    return(tibble::tibble(date = NA_real_, sum = NA_real_))
  }

  grid <- make_year_grid(stepstart, stepstop)
  years <- grid$years
  n_grid <- grid$stop_int - grid$start_int + 1L

  # Drop NA pairs, but do NOT clamp yet (unique periods should be based on original bounds)
  iv0 <- clean_intervals(from, to, -Inf, Inf)
  f0 <- iv0$f
  t0 <- iv0$t

  if (length(f0) == 0) {
    return(tibble::tibble(date = years, sum = NA_real_))
  }

  # Unique periods and how often they occur
  periods <- data.frame(from = f0, to = t0)
  unique_periods <- unique(periods)

  # robust 2-col key mapping without paste()
  pid <- match(
    interaction(periods$from, periods$to, drop = TRUE),
    interaction(unique_periods$from, unique_periods$to, drop = TRUE)
  )
  counts <- tabulate(pid, nbins = nrow(unique_periods))

  # Clamp unique periods to window for indexing
  uf <- pmax(as.integer(unique_periods$from), grid$start_int)
  ut <- pmin(as.integer(unique_periods$to),   grid$stop_int)

  keep <- uf <= ut
  if (!all(keep)) {
    uf <- uf[keep]
    ut <- ut[keep]
    counts <- counts[keep]
  }

  U <- length(uf)
  if (U == 0) {
    return(tibble::tibble(date = years, sum = NA_real_))
  }

  # Indices for unique periods
  i_start <- uf - grid$start_int + 1L
  i_end   <- ut - grid$start_int + 1L

  # 1) n_periods[y] = number of UNIQUE periods covering year y
  n_periods <- diff_prefix_sum(
    i_start = i_start,
    i_end   = i_end,
    values  = rep.int(1L, U),
    n_grid  = n_grid
  )

  # 2) accumulate final ao sum without ao_weight matrix
  ao_sum <- numeric(n_grid)

  for (i in seq_len(U)) {
    if (counts[i] == 0L) next

    idx <- i_start[i]:i_end[i]

    base_w <- if (correct) (1 / n_periods[idx]) else rep.int(1, length(idx))

    denom <- sum(base_w)
    if (!is.finite(denom) || denom == 0) next

    ao_sum[idx] <- ao_sum[idx] + counts[i] * (base_w / denom)
  }

  tibble::tibble(date = years, sum = ao_sum)
}