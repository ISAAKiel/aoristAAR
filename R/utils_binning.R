#' @noRd
validate_align_to <- function(align_to) {
  if (is.null(align_to)) return(0L)

  if (!is.numeric(align_to) || length(align_to) != 1) {
    stop("`align_to` must be NULL or a single number.", call. = FALSE)
  }

  if (is.na(align_to)) {
    stop("`align_to` must not be NA.", call. = FALSE)
  }

  align_to <- as.integer(align_to)
  if (align_to < 0L) stop("`align_to` must be >= 0.", call. = FALSE)
  align_to
}

#' @noRd
bin_ts_from_yearly <- function(yearly, stepwidth, align_to = NULL, aggregate = FALSE) {
  stepwidth <- validate_stepwidth(stepwidth)

  # NULL = legacy "no alignment"; numeric (incl 0) = alignment anchor
  align_to_i <- if (is.null(align_to)) NULL else validate_align_to(align_to)

  if (!all(c("date", "sum") %in% names(yearly))) {
    stop("`yearly` must have columns `date` and `sum`.", call. = FALSE)
  }

  y_date <- as.integer(yearly$date)
  y_sum  <- na_to_zero(yearly$sum)

  y_min <- min(y_date, na.rm = TRUE)
  y_max <- max(y_date, na.rm = TRUE)

  if (is.null(align_to_i)) {
    # legacy: no alignment, range follows data
    start <- y_min
    stop  <- y_max
  } else {
    # alignment: bins are anchored at align_to_i and spaced by stepwidth
    anchor <- align_to_i
    start <- anchor + as.integer(floor((y_min - anchor) / stepwidth) * stepwidth)
    stop  <- anchor + as.integer(ceiling((y_max - anchor) / stepwidth) * stepwidth)
  }

  out_dates <- seq(start, stop, by = stepwidth)

  # build dense yearly vector over [start, stop]
  n_grid <- stop - start + 1L
  dense <- numeric(n_grid)
  idx <- y_date - start + 1L
  ok <- idx >= 1L & idx <= n_grid
  dense[idx[ok]] <- y_sum[ok]

  # legacy sampling only when NO alignment and NO aggregation requested
  if (is.null(align_to_i) && !isTRUE(aggregate)) {
    out_idx <- out_dates - start + 1L
    out_sum <- dense[out_idx]
    return(tibble::tibble(date = out_dates, sum = out_sum))
  }

  # otherwise: aggregate into bins [d, d+stepwidth-1]
  out_sum <- numeric(length(out_dates))
  for (k in seq_along(out_dates)) {
    d0 <- out_dates[k]
    d1 <- min(d0 + stepwidth - 1L, stop)
    i0 <- d0 - start + 1L
    i1 <- d1 - start + 1L
    out_sum[k] <- sum(dense[i0:i1])
  }

  tibble::tibble(date = out_dates, sum = out_sum)
}