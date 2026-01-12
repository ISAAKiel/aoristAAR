#' @noRd
validate_method <- function(method) {
  allowed <- c("number","weight","period_correction")
  if (!method %in% allowed) stop(...)
  method
}

#' @noRd
na_to_zero <- function(x) { x[is.na(x)] <- 0; x }

#' @noRd
zero_to_na <- function(x) {
  x[x == 0] <- NA_real_
  x
}

#' @noRd
validate_stepwidth <- function(stepwidth) {
  if (!is.numeric(stepwidth) || length(stepwidth) != 1 || stepwidth <= 0) {
    stop("`stepwidth` must be a single positive number.", call. = FALSE)
  }
  as.integer(stepwidth)
}

#' @noRd
compute_range <- function(x, from, to) {
  f <- suppressWarnings(as.integer(x[[from]]))
  t <- suppressWarnings(as.integer(x[[to]]))

  ok <- !(is.na(f) | is.na(t))
  f <- f[ok]
  t <- t[ok]

  if (length(f) == 0) {
    return(list(stepstart = NA_integer_, stepstop = NA_integer_))
  }

  # IMPORTANT: do NOT snap to stepwidth, do NOT align outward.
  # Use the real observed integer year range.
  stepstart <- min(pmin(f, t))
  stepstop  <- max(pmax(f, t))

  list(stepstart = stepstart, stepstop = stepstop)
}

#' @noRd
empty_aorist_ts <- function(method, stepwidth, from, to, split_vars, n_records, call) {
  new_aorist_ts(
    tibble::tibble(date = NA_real_, sum = 0),
    method = method, stepwidth = stepwidth,
    from = from, to = to, split_vars = split_vars,
    stepstart = NA_real_, stepstop = NA_real_,
    n_records = n_records,
    call = call
  )
}

#' @noRd
validate_columns <- function(x, cols) {
  miss <- setdiff(cols, names(x))
  if (length(miss) > 0) {
    stop("Column(s) not found in `x`: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
validate_interval <- function(interval) {
  allowed <- c("[]", "[)", "(]", "()")
  if (!is.character(interval) || length(interval) != 1) {
    stop("`interval` must be a single character string.", call. = FALSE)
  }
  if (!interval %in% allowed) {
    stop("Unknown `interval`. Must be one of: ", paste(allowed, collapse = ", "), call. = FALSE)
  }
  interval
}

#' @noRd
validate_calendar <- function(calendar) {
  allowed <- c("astronomical", "historical")
  if (!is.character(calendar) || length(calendar) != 1) {
    stop("`calendar` must be a single character string.", call. = FALSE)
  }
  if (!calendar %in% allowed) {
    stop("Unknown `calendar`. Must be one of: ", paste(allowed, collapse = ", "), call. = FALSE)
  }
  calendar
}

#' @noRd
validate_calendar_year0 <- function(from, to, calendar) {
  calendar <- validate_calendar(calendar)
  if (identical(calendar, "historical")) {
    f <- as.integer(from)
    t <- as.integer(to)
    # reject any explicit 0 in either bound (NAs ignored)
    if (any(f == 0L, na.rm = TRUE) || any(t == 0L, na.rm = TRUE)) {
      stop("calendar='historical' does not allow year 0.", call. = FALSE)
    }
  }
  invisible(TRUE)
}