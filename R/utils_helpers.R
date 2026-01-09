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
  stepstart <- min(x[[from]], na.rm = TRUE)
  stepstop  <- max(x[[to]],   na.rm = TRUE)
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