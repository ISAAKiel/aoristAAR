#' @noRd
compute_one_group <- function(dat, from, to,
                              stepwidth, stepstart, stepstop,
                              method, split_vars,
                              interval = "[]",
                              calendar = "astronomical") {

  # Always compute YEARLY series here (no binning/regridding in this function)
  ts <- seq2ts(
    from = dat[[from]],
    to   = dat[[to]],
    stepstart = stepstart,
    stepstop  = stepstop,
    stepwidth = stepwidth,
    method    = method,
    interval  = interval,
    calendar  = calendar
  )

  if (length(split_vars) > 0) {
    for (v in split_vars) ts[[v]] <- dat[[v]][1]
  }

  ts
}