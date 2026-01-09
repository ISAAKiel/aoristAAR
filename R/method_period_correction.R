# weighting by period attribution
method_period_correction <- function(from, to, stepwidth, stepstart, stepstop, correct = TRUE) {

  if (stepwidth != 1) {
    stop("Method 'period_correction' only works with stepwidth = 1.", call. = FALSE)
  }

  # handle all-NA early (align with your seq2ts guard)
  if (all(is.na(from)) || all(is.na(to))) {
    return(tibble::tibble(date = NA_real_, sum = NA_real_))
  }

  # integer grid
  start_int <- as.integer(stepstart)
  stop_int  <- as.integer(stepstop)
  years <- start_int:stop_int
  Tn <- length(years)

  f <- as.integer(from)
  t <- as.integer(to)
  ok <- !(is.na(f) | is.na(t))
  f <- f[ok]; t <- t[ok]
  if (length(f) == 0) {
    return(tibble::tibble(date = years, sum = NA_real_))
  }

  # build unique periods and how often they occur (this replaces period_id logic)
  periods <- data.frame(from = f, to = t)
  # unique() keeps first occurrence; use match to map rows to unique periods
  unique_periods <- unique(periods)
  pid <- match(paste(periods$from, periods$to), paste(unique_periods$from, unique_periods$to))
  counts <- tabulate(pid, nbins = nrow(unique_periods))  # frequency of each unique period

  # clamp periods to the global window for indexing
  uf <- pmax(as.integer(unique_periods$from), start_int)
  ut <- pmin(as.integer(unique_periods$to),   stop_int)

  # drop unique periods that do not overlap the window at all
  keep <- uf <= ut
  if (!all(keep)) {
    unique_periods <- unique_periods[keep, , drop = FALSE]
    uf <- uf[keep]; ut <- ut[keep]
    counts <- counts[keep]
  }

  U <- nrow(unique_periods)
  if (U == 0) {
    return(tibble::tibble(date = years, sum = NA_real_))
  }

  # 1) compute n_periods[y] = number of unique periods covering year y
  # using a difference array (O(U + T))
  diff <- integer(Tn + 1L)
  i_start <- uf - start_int + 1L
  i_end   <- ut - start_int + 1L
  diff[i_start] <- diff[i_start] + 1L
  endp1 <- i_end + 1L
  in_range <- endp1 <= length(diff)
  diff[endp1[in_range]] <- diff[endp1[in_range]] - 1L
  n_periods <- cumsum(diff)[seq_len(Tn)]  # length Tn

  # 2) accumulate final ao sum without creating ao_weight matrix
  ao_sum <- numeric(Tn)

  for (i in seq_len(U)) {
    if (counts[i] == 0L) next

    idx0 <- i_start[i]
    idx1 <- i_end[i]
    idx <- idx0:idx1

    if (correct) {
      base_w <- 1 / n_periods[idx]
    } else {
      base_w <- rep(1, length(idx))
    }

    denom <- sum(base_w)
    if (!is.finite(denom) || denom == 0) next

    ao_sum[idx] <- ao_sum[idx] + counts[i] * (base_w / denom)
  }

  tibble::tibble(date = years, sum = ao_sum)
}