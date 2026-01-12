#' @noRd
seq2ts <- function(from, to,
                   stepwidth, stepstart, stepstop,
                   method = "number",
                   interval = "[]",
                   calendar = "astronomical",
                   align_to = 0,
                   aggregate = FALSE) {

  if (all(is.na(from)) || all(is.na(to))) {
    return(tibble::tibble(date = NA_real_, sum = NA_real_))
  }

  method   <- validate_method(method)
  interval <- validate_interval(interval)
  calendar <- validate_calendar(calendar)
  align_to <- validate_align_to(align_to)

  validate_calendar_year0(from, to, calendar)

  # period_correction stays special: must be yearly
  if (method == "period_correction") {
    if (stepwidth != 1) {
      stop("Method 'period_correction' only works with stepwidth = 1.", call. = FALSE)
    }
    return(method_period_correction(
      from = from, to = to,
      stepwidth = 1,
      stepstart = stepstart,
      stepstop  = stepstop,
      correct = TRUE
    ))
  }

  # 1) build YEARLY curve (stepwidth = 1) always
  yearly <- switch(
    method,
    number = method_number(
      from = from, to = to,
      stepwidth = 1,
      stepstart = stepstart,
      stepstop  = stepstop,
      interval = interval
    ),
    weight = method_weight(
      from = from, to = to,
      stepwidth = 1,
      stepstart = stepstart,
      stepstop  = stepstop,
      interval = interval
    )
  )

  # 2) fast path: already yearly and no alignment/bins requested
  if (stepwidth == 1 && align_to == 0 && !isTRUE(aggregate)) {
    return(yearly)
  }

  # 3) bin/sample from yearly curve
  bin_ts_from_yearly(
    yearly = yearly,
    stepwidth = stepwidth,
    align_to = align_to,
    aggregate = isTRUE(aggregate)
  )
}