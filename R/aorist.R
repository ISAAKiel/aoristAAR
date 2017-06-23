#' Calculates an aoristic sum based on a start and end date
#'
#' Calculates an aoristic sum based on a start and end date
#'
#' @param from vector of start dates
#' @param to vector of end dates
#' @param correct apply a correction for overlapping phases (see details)
#'
#' @return a vector containing the aoristic sum for the entered data
#'
#' @examples
#'
#' # to come
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