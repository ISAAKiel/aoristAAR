#' Calculates a time series based on a start and end date
#'
#' Calculates a time series of unit-wise (e.g. year-wise) occurrence based on a start and end date.
#' Dates BC(E) have to be given with negative sign.
#'
#' @param x Data.frame.
#' @param from Character or Integer. Names or indizes of "from" column (start date) in x.
#' @param to Character or Integer. Names or indizes of "to" column (end date) in x.
#' @param split_vars Character Vector or Integer Vector. Names or indizes of columns by which the
#' x should be split before time series creation. Can be a vector of multiple values.
#' @param stepwidth Integer. Width of each time step in the resulting time series. Default = 1.
#' Can not be changed if method = "period_correction".
#' @param stepstart Integer. Start of the time window of interest. Default = \code{min(from, na.rm = T)}.
#' @param stepstop Integer. End of the time window of interest. Default = \code{max(to, na.rm = T)}.
#'
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
#' multiple time periods.
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
#' lines(method_period_correction_time_series, type = "l", col = "orange", lty = 2)
#' legend(
#'   -4100, 0.04, legend = c("hamlets", "hillforts"),
#'   col = c("darkgreen", "orange"), lty = 1, cex = 0.8
#' )
#'
#' @importFrom Rdpack reprompt
#'
#' @export
aorist <- function(
  x,
  from = "from",
  to = "to",
  split_vars = c(),
  stepwidth = 1,
  stepstart = min(x[[from]], na.rm = T),
  stepstop = max(x[[to]], na.rm = T),
  method = "number"
) {

  if (length(split_vars) > 0) {

    x_split <- base::split(x, lapply(split_vars, function(y) { x[[y]] }))
    x_split_exist <- x_split[sapply(x_split, function(x) { nrow(x) > 0})]
    x_list <- x_split_exist

    artefact_timeseries <- pbapply::pblapply(
      names(x_list),
      function(nx, split_vars, stepwidth, method) {
        nox <- x_list[[nx]]
        timeseries <- seq2ts(
          nox[[from]],
          nox[[to]],
          stepwidth = stepwidth,
          stepstart = stepstart,
          stepstop = stepstop,
          method = method
        )
        for (i in split_vars) { timeseries[[i]] <- nox[[i]][1] }
        return(timeseries)
      },
      split_vars = split_vars,
      stepwidth = stepwidth,
      method = method
    )

    artefact_timeseries_df <- do.call(rbind, artefact_timeseries)

  } else {

    artefact_timeseries_df <- seq2ts(
      x[[from]],
      x[[to]],
      stepwidth = stepwidth,
      stepstart = stepstart,
      stepstop = stepstop,
      method = method
    )

  }

  # replace NA values with 0: Non-occurrence equals zero
  artefact_timeseries_df$sum[is.na(artefact_timeseries_df$sum)] <- 0

  return(artefact_timeseries_df)

}

#### method switch ####

seq2ts <- function(from, to, stepwidth, stepstart, stepstop, method = "number") {

  if (all(is.na(from)) | all(is.na(to))) {
    return(tibble::tibble(date = NA_real_, sum = NA_real_))
  }

  return(
    switch (method,
      "number" = method_number(
        from, to,
        stepwidth = stepwidth, stepstart = stepstart, stepstop = stepstop
      ),
      "weight" = method_weight(
        from, to,
        stepwidth = stepwidth, stepstart = stepstart, stepstop = stepstop
      ),
      "period_correction" = method_period_correction(
        from, to,
        stepwidth = stepwidth, stepstart = stepstart, stepstop = stepstop
      )
    )
  )

}

#### methods ####

# simple counting of occurrence
method_number <- function(from, to, stepwidth, stepstart, stepstop) {

  input <- tibble::tibble(from, to)

  output <- tibble::tibble(date = seq(stepstart, stepstop, by = stepwidth))
  output$sum <- sapply(
    output$date,
    function(y, x) {
      nrow(subset(x, x$from <= y & x$to >= y))
    },
    input
  )
  output$sum[output$sum == 0] <- NA_real_

  return(output)
}

# weighting by dating precision
method_weight <- function(from, to, stepwidth, stepstart, stepstop) {
  input <- tibble::tibble(from, to)
  input$number_of_years <- abs(input$from - input$to)
  input$number_of_years <- ifelse(input$number_of_years == 0, 1, input$number_of_years)
  input$weight_per_year = 1/input$number_of_years

  output <- tibble::tibble(date = seq(stepstart, stepstop, by = stepwidth))
  output$sum <- sapply(
    output$date,
    function(y, x) {
      sum(subset(x, x$from <= y & x$to >= y)$weight_per_year)
    },
    input
  )
  output$sum[output$sum == 0] <- NA_real_

  return(output)
}

# weighting by period attribution
method_period_correction <- function(from, to, stepwidth, stepstart, stepstop, correct = T) {

  if (stepwidth != 1) {
    stop("Method 'perdiod_correction' only works with stepwidth = 1.")
  }

  dates <- as.data.frame(cbind(from,to))
  unique_periodes <- unique(dates)
  unique_periodes$id <- 1:nrow(unique_periodes)

  dates$period_id <- apply(dates,1,function(x)
    unique_periodes$id[unique_periodes$from==x[1] & unique_periodes$to==x[2]])

  time_window <- c(stepstart, stepstop)

  n_periods <- data.frame(date = time_window[1]:time_window[2], sum=0)

  for (i in 1:nrow(unique_periodes)) {
    this_index <- n_periods$date>=unique_periodes$from[i] & n_periods$date<=unique_periodes$to[i]
    n_periods$sum[this_index] <- n_periods$sum[this_index] + 1
  }

  ao_weight <- matrix(nrow = nrow(unique_periodes), ncol=nrow(n_periods))

  for (i in 1:nrow(unique_periodes)) {
    this_period <- unique_periodes[i,]
    ao_weight[i,]<-rep(0,nrow(n_periods))
    within_period <- n_periods$date>=this_period$from & n_periods$date<=this_period$to
    if (correct) {
      ao_weight[i,within_period]<-1/n_periods$sum[within_period]
    } else {
      ao_weight[i,within_period]<-1
    }
    ao_weight[i,]<-ao_weight[i,]/sum(ao_weight[i,])
  }

  ao_weight[is.nan(ao_weight)] <- 0

  ao_sum_collector <- rep(0,nrow(n_periods))

  for (i in 1:nrow(dates)) {
    ao_sum_collector<-ao_sum_collector+ao_weight[unique_periodes$id==dates$period_id[i]]
  }

  final_ao_sum <- tibble::tibble(date = n_periods$date, sum = ao_sum_collector)

  return(final_ao_sum)
}
