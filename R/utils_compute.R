#' @noRd
compute_one_group <- function(dat, from, to, stepwidth, stepstart, stepstop, method, split_vars) {
  ts <- seq2ts(
    dat[[from]],
    dat[[to]],
    stepwidth = stepwidth,
    stepstart = stepstart,
    stepstop  = stepstop,
    method = method
  )

  if (length(split_vars) > 0) {
    for (v in split_vars) ts[[v]] <- dat[[v]][1]
  }
  ts
}