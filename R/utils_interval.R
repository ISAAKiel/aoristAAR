#' @noRd
apply_interval_closure <- function(f, t, interval = "[]") {
  # f, t are integer vectors (already NA-cleaned, typically already clamped)
  if (length(f) == 0) return(list(f = integer(), t = integer(), keep = logical()))

  if (!is.character(interval) || length(interval) != 1) {
    stop("`interval` must be a single character string.", call. = FALSE)
  }

  if (identical(interval, "[]")) {
    # inclusive-inclusive: do nothing
  } else if (identical(interval, "[)")) {
    # include start, exclude end
    t <- t - 1L
  } else if (identical(interval, "(]")) {
    # exclude start, include end
    f <- f + 1L
  } else if (identical(interval, "()")) {
    # exclude both
    f <- f + 1L
    t <- t - 1L
  } else {
    stop("Unknown `interval`. Must be one of: [], [), (], ().", call. = FALSE)
  }

  keep <- f <= t
  list(f = f[keep], t = t[keep], keep = keep)
}