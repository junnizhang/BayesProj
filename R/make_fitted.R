
## HAS_TESTS
#' Fit Time Series Model
#'
#' The function that does the actual
#' fitting, via TMB.
#'
#' @param inputs A data frame containing
#' the the indicator (`val`) as a list
#' of lists, and the 'by' variable(s)
#' if any.
#' @param spec_ts. Specification of the
#' time series model. An object of
#' class `"BayesProj_spec_ts"`.
#'
#' @returns A list of lists,
#' containing elements `mean`
#' and `prec`.
#' 
#' @noRd
make_fitted <- function(inputs, spec_ts) {
  vals <- inputs[["val"]]
  y1 <- vals[[1L]][[1L]]
  class_spec <- make_class_spec(spec_ts)
  parameters <- make_parameters(spec = spec_ts, y = y1)
  random <- make_random(spec_ts)
  for (i_val in seq_along(vals)) {
    val <- vals[[i_val]]
    for (i_y in seq_along(val)) {
      y <- val[[i_y]]
      consts <- make_consts(spec = spec_ts, y = y)
      is_in_lik <- as.integer(!is.na(y))
      data <- list(y = y,
                   is_in_lik = is_in_lik,
                   class_spec = class_spec,
                   consts = consts)
      f <- TMB::MakeADFun(data = data,
                          parameters = parameters,
                          random = random,
                          DLL = "BayesProj",
                          silent = TRUE)
      stats::nlminb(start = f$par,
                    objective = f$fn,
                    gradient = f$gr,
                    silent = TRUE)
      sdreport <- TMB::sdreport(f,
                                bias.correct = TRUE,
                                getJointPrecision = TRUE)
      mean <- as.list(sdreport, what = "Est")
      attr(mean, "what") <- NULL
      prec <- sdreport$jointPrecision
      val[[i_y]] <- list(mean = mean,
                         prec = prec)
    }
    vals[[i_val]] <- val
  }
  vals
}

