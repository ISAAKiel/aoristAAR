#### method switch ####

seq2ts <- function(from, to, stepwidth, stepstart, stepstop, method = "number") {

  if (all(is.na(from)) || all(is.na(to))) {
    return(tibble::tibble(date = NA_real_, sum = NA_real_))
  }

  allowed_methods <- c("number", "weight", "period_correction")
  if (!method %in% allowed_methods) {
    stop("Unknown `method`. Must be one of: ",
         paste(allowed_methods, collapse = ", "),
         call. = FALSE)
  }

  args <- list(
    from = from, to = to,
    stepwidth = stepwidth,
    stepstart = stepstart,
    stepstop = stepstop
  )

  switch(
    method,
    number = do.call(method_number, args),
    weight = do.call(method_weight, args),
    period_correction = do.call(method_period_correction, args)
  )
}