#' Calculates an aoristic sum based on a start and end date
#'
#' Calculates an aoristic sum based on a start and end date. Dates BC(E) have to be given with negative sign.
#'
#' @param from vector of start dates
#' @param to vector of end dates
#' @param correct apply a correction for overlapping phases (see details)
#'
#' @return a vector containing the aoristic sum for the entered data
#'
#' @details According to Mischka (2004), Aoristic analysis 'is a method used in criminology to analyse crime incidents and determine probabilities for the contemporaneity of the incidents or, when applied to archaeology, for the contemporaneity of sites'.
#'
#' The aoristic calculation distributes the probability of an event (the event has taken place at all = 1) to (time) sections of the total range within which the event may have taken place. The length of these periods can be arbitrarily chosen, for archaeological applications we set them at the annual level. This scaling can easily be brought to the desired scale by aggregation.
#'
#' The aoristic sum is then the expected value for the number of events to be assumed within this period. This can be, for example, the expected number of settlements that have been included in the calculation as individual data with start and end date as parameters.
#'
#' The calculation of the aoristic sum is based on exclusive time intervals in its original implementation (Radcliffe 2000). In archaeological applications, however, overlapping time intervals often result from different dating accuracy. For example, individual sites may only be categorized as part of the Neolithic, others may be narrowed down to the Middle Neolithic Ia. The structure of the overlapping time intervals can lead to biases of the aoristic sum (Hinz/Müller-Scheeßel forthcoming), which is corrected by the algorithm by weighting down multiple time periods.
#'
#' @references {
#'
#'   \insertRef{ratcliffe_aoristic_2000}{aoristAAR}
#'
#'   \insertRef{mischka_aoristische_2004}{aoristAAR}
#'
#'   \insertRef{hinz_systematic_nodate}{aoristAAR}
#' }
#'
#' @examples
#' my_settlements <- data.frame(from = c(-3800, -3750, -3500),
#'                              to   = c(-3700, -3400, -3300))
#'
#' my_aoristic_sum <- aorist(from = my_settlements$from,
#'                           to   = my_settlements$to)
#'
#' plot(my_aoristic_sum,
#'      type="l",
#'      xlim = c(-4100, -3200),
#'      ylim = c(0,0.02))
#'
#' @importFrom Rdpack reprompt
#'
#' @export

aorist <- function(from, to, correct=F) {

  dates <- as.data.frame(cbind(from,to))
  unique_periodes <- unique(dates)
  unique_periodes$id <- 1:nrow(unique_periodes)

  dates$period_id <- apply(dates,1,function(x)
    unique_periodes$id[unique_periodes$from==x[1] & unique_periodes$to==x[2]])

  time_window <- c(min(dates$from), max(dates$to))

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

  ao_sum_collector <- rep(0,nrow(n_periods))

  for (i in 1:nrow(dates)) {
    ao_sum_collector<-ao_sum_collector+ao_weight[unique_periodes$id==dates$period_id[i]]
  }

  final_ao_sum <- data.frame(date = n_periods$date, sum = ao_sum_collector)

  return(final_ao_sum)
}