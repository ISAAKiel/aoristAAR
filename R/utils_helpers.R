#' @noRd
validate_method <- function(method) {
  allowed <- c("number","weight","period_correction")
  if (!method %in% allowed) stop(...)
  method
}

#' @noRd
na_to_zero <- function(x) { x[is.na(x)] <- 0; x }