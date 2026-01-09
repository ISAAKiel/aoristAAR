#' Internal constructor for aorist time series objects
#'
#' @noRd
new_aorist_ts <- function(df,
                          method,
                          stepwidth,
                          from,
                          to,
                          split_vars,
                          stepstart,
                          stepstop,
                          n_records,
                          call = NULL) {

  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame.", call. = FALSE)
  }
  if (!all(c("date", "sum") %in% names(df))) {
    stop("`df` must contain columns `date` and `sum`.", call. = FALSE)
  }

  structure(
    df,
    class = c("aorist_ts", class(df)),
    method = method,
    stepwidth = stepwidth,
    from = from,
    to = to,
    split_vars = split_vars,
    stepstart = stepstart,
    stepstop = stepstop,
    n_records = n_records,
    call = call
  )
}

#' Check whether an object is an aorist time series
#'
#' @param x Any object.
#' @return Logical.
#' @export
is_aorist_ts <- function(x) inherits(x, "aorist_ts")