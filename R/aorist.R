#' Calculates a time series based on a start and end date
#'
#' Calculates a time series of unit-wise (e.g. year-wise) occurrence based on a start and end date.
#' Dates BC(E) have to be given with negative sign.
#'
#' @param x Data.frame.
#' @param from Character or Integer. Names or indices of "from" column (start date) in x.
#' @param to Character or Integer. Names or indices of "to" column (end date) in x.
#' @param split_vars Character Vector or Integer Vector. Names or indices of columns by which the
#' x should be split before time series creation. Can be a vector of multiple values.
#' @param stepwidth Integer. Width of each time step in the resulting time series. Default = 1.
#' Can not be changed if method = "period_correction".
#' @param method Character. Method switch to decide how the sum per timestep should be calculated.
#' \itemize{
#'   \item{"number": }{Number of elements within one timestep.}
#'   \item{"weight": }{Sum of weighted occurences. Weighting considers the dating precision/length of periods.}
#'   \item{"period_correction": }{More complex weighting method. See the section below.}
#' }
#'
#' @return Tibble (data.frame) with one row for each timestep and at least two columns:
#' \itemize{
#'   \item{date: }{Timestep.}
#'   \item{sum: }{Calculated sum values per timestep.}
#'   \item{... : }{Type variables if \code{split_vars} was set.}
#' }
#'
#' @section Aoristic period correction with method = "period_correction":
#' According to Mischka (2004), Aoristic analysis 'is a method used in criminology to
#' analyse crime incidents and determine probabilities for the contemporaneity of the incidents
#' or, when applied to archaeology, for the contemporaneity of sites'.
#'
#' The aoristic calculation distributes the probability of an event (the event has taken place
#' at all = 1) to (time) sections of the total range within which the event may have taken place.
#' The length of these periods can be arbitrarily chosen, for archaeological applications we set
#' them at the annual level. This scaling can easily be brought to the desired scale by aggregation.
#'
#' The aoristic sum is then the expected value for the number of events to be assumed within this
#' period. This can be, for example, the expected number of settlements that have been included
#' in the calculation as individual data with start and end date as parameters.
#'
#' The calculation of the aoristic sum is based on exclusive time intervals in its original
#' implementation (Radcliffe 2000). In archaeological applications, however, overlapping time
#' intervals often result from different dating accuracy. For example, individual sites may only
#' be categorized as part of the Neolithic, others may be narrowed down to the Middle Neolithic Ia.
#' The structure of the overlapping time intervals can lead to biases of the aoristic sum
#' (Hinz/Müller-Scheeßel forthcoming), which is corrected by the algorithm by weighting down
#' multiple time periods. If the 'period_correction' method has been selected
#' the aoristic sum per time period, selected by stepwidth, is divided by
#' the number of overlapping periods for this time period. This method is
#' rather simplistic, but in many cases leads to a better estimate.
#' A more elaborate version is under development.
#'
#' @references {
#'   \insertRef{ratcliffe_aoristic_2000}{aoristAAR}
#'
#'   \insertRef{mischka_aoristische_2004}{aoristAAR}
#'
#'   \insertRef{hinz_systematic_nodate}{aoristAAR}
#' }
#'
#' @examples
#' # creating test data
#' my_settlements <- data.frame(
#'   start = c(-3800, -3750, -3500, -4000, -3800, -3800, -3550, -3750, -3800),
#'   end   = c(-3700, -3400, -3300, -3300, -3500, -3300, -3525, -3650, -3700),
#'   type = c("hillfort", "hillfort", "hillfort", "hillfort", "hillfort",
#'   "coastal settlement", "coastal settlement", "coastal settlement", "coastal settlement"),
#'   size = c(">2ha", "<2ha", ">2ha", ">2ha", "<2ha", ">2ha", "<2ha", ">2ha", "<2ha")
#' )
#'
#' # counting number of occurences
#' method_number_time_series <- aorist(
#'   my_settlements,
#'   from = "start", to = "end",
#'   method = "number"
#' )
#' plot(method_number_time_series, type = "l")
#'
#' # normalisation methods
#' method_weight_time_series <- aorist(
#'   my_settlements,
#'   from = "start", to = "end",
#'   method = "weight"
#' )
#' method_period_correction_time_series <- aorist(
#'   my_settlements,
#'   from = "start", to = "end",
#'   method = "period_correction"
#' )
#'
#' plot(method_weight_time_series, type = "l", col = "blue", xlim = c(-4100, -3200))
#' lines(method_period_correction_time_series, type = "l", col = "red", lty = 2)
#' legend(
#'   -4100, 0.05, legend = c("weight", "period_correction"),
#'   col = c("blue", "red"), lty = 1:2, cex = 0.8
#' )
#'
#' # splitting time series by additional variables
#' splitted_time_series <- aorist(
#'   my_settlements,
#'   from = "start", to = "end",
#'   split_vars = c("type"),
#'   method = "period_correction"
#' )
#'
#' hamlets <- splitted_time_series[splitted_time_series$type == "coastal settlement", c(1,2)]
#' hillforts <- splitted_time_series[splitted_time_series$type == "hillfort", c(1,2)]
#'
#' plot(hamlets, type = "l", col = "darkgreen", xlim = c(-4100, -3200))
#' lines(hillforts, type = "l", col = "darkblue", lty = 2)
#' legend(
#'   -4100, 0.04, legend = c("hamlets", "hillforts"),
#'   col = c("darkgreen", "orange"), lty = 1, cex = 0.8
#' )
#'
#' @importFrom Rdpack reprompt
#'
#' @export
aorist <- function(x,
                   from = "from",
                   to = "to",
                   split_vars = character(),
                   stepwidth = 1,
                   method = "number",
                   interval = "[]",
                   calendar = "astronomical",
                   align_to = NULL,
                   aggregate = FALSE) {

  if (!is.data.frame(x)) stop("`x` must be a data.frame.", call. = FALSE)

  validate_columns(x, c(from, to))
  if (length(split_vars) > 0) validate_columns(x, split_vars)

  # Keep these validations, but ONLY use align_to later if we actually bin.
  stepwidth <- validate_stepwidth(stepwidth)
  method    <- validate_method(method)
  interval  <- validate_interval(interval)
  calendar  <- validate_calendar(calendar)

  # Important: preserve "missing align_to" as NULL (no alignment requested)
  align_to_arg <- if (missing(align_to)) NULL else align_to
  if (!is.null(align_to_arg)) align_to_arg <- validate_align_to(align_to_arg)

  validate_calendar_year0(x[[from]], x[[to]], calendar)

  # all-NA early return
  if (all(is.na(x[[from]])) || all(is.na(x[[to]]))) {
    return(empty_aorist_ts(
      method = method, stepwidth = stepwidth,
      from = from, to = to, split_vars = split_vars,
      n_records = nrow(x),
      call = match.call()
    ))
  }

  # YEARLY range (independent of binning)
  r <- compute_range(x, from, to)
  stepstart <- r$stepstart
  stepstop  <- r$stepstop

  # --- 1) Compute YEARLY series only (no binning here) ---
  compute_one <- function(dat) {
    compute_one_group(
      dat, from, to,
      stepwidth = stepwidth,
      stepstart = stepstart,
      stepstop  = stepstop,
      method    = method,
      split_vars = split_vars,
      interval  = interval,
      calendar  = calendar
    )
  }

  if (length(split_vars) > 0) {
    x_split <- split(x, x[split_vars], drop = TRUE)
    artefact_timeseries_df <- do.call(rbind, lapply(x_split, compute_one))
    artefact_timeseries_df <- tibble::as_tibble(artefact_timeseries_df)
  } else {
    artefact_timeseries_df <- compute_one(x)
  }

  # treat NA sums as zero (contract expects no NA in sum)
  artefact_timeseries_df$sum <- na_to_zero(artefact_timeseries_df$sum)

  # --- 2) Optional binning/regridding happens ONLY here ---
  needs_binning <- !(stepwidth == 1 && is.null(align_to_arg) && !isTRUE(aggregate))

  if (needs_binning) {

    bin_one_ts <- function(ts) {
      # ts is yearly with columns date,sum
      b <- bin_ts_from_yearly(
        yearly    = ts,
        stepwidth = stepwidth,
        align_to  = align_to_arg,
        aggregate = isTRUE(aggregate)
      )
      b$sum <- na_to_zero(b$sum)
      b
    }

    if (length(split_vars) > 0) {
      # split the *yearly time series output* by split_vars, bin each, then reattach keys
      out <- split(artefact_timeseries_df, artefact_timeseries_df[split_vars], drop = TRUE)
      out <- lapply(out, function(ts) {
        keys <- ts[1, split_vars, drop = FALSE]
        b <- bin_one_ts(ts[, c("date", "sum")])
        for (v in split_vars) b[[v]] <- keys[[v]]
        b
      })
      artefact_timeseries_df <- tibble::as_tibble(do.call(rbind, out))
    } else {
      artefact_timeseries_df <- bin_one_ts(artefact_timeseries_df)
    }
  }

  new_aorist_ts(
    artefact_timeseries_df,
    method = method,
    stepwidth = stepwidth,
    from = from,
    to = to,
    split_vars = split_vars,
    stepstart = stepstart,
    stepstop = stepstop,
    n_records = nrow(x),
    call = match.call()
  )
}