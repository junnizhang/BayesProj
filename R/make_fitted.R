
#' Fit Time Series Model
#'
#' The function that does the actual
#' fitting, via TMB.
#'
#' @param inputs A data frame containing
#' the the indicator (`val`) as a list
#' of lists, and the 'by' variable(s)
#' if any.
#' @param spec. Specification of the
#' time series model. An object of
#' class `"BayesProj_spec_ts"`.
#'
#' @returns A list of lists,
#' containing elements `mean`
#' and `prec`.
#' 
#' @noRd
make_fitted <- function(inputs, spec) {
  vals <- inputs[["val"]]
  n_period <- length(val[[1L]][[1L]])
  class_spec <- make_class_spec(spec)
  par <- make_par(spec = spec, n_period = n_period)
  hyper <- make_hyper(spec)
  parameters <- list(par = par,
                     hyper = hyper)
  random <- make_random(spec_ts)
  for (i_val in seq_along(vals)) {
    val <- vals[[i_val]]
    for (i_y in seq_along(val)) {
      y <- val[[i_y]]
      consts <- make_consts(spec = spec, y = y)
      is_in_lik <- as.integer(!is.na(y))
      data <- list(y = y,
                   is_in_lik = is_in_lik,
                   class_spec = class_spec,
                   consts = consts)
      f <- TMB::MakeADFun(data = data,
                          parameters = parameters,
                          DLL = "BayesProj",
                          random = random,
                          silent = TRUE)
      stats::nlminb(start = f$par,
                    objective = f$fn,
                    gradient = f$gr)
      sdreport <- TMB::sdreport(f,
                                bias.correct = TRUE,
                                getJointPrecision = TRUE)
      mean <- c(sdreport$par.fixed, sdreport$par.random)
      prec <- sdreport$jointPrecision
      val[[i_y]] <- list(mean = mean,
                         prec = prec)
    }
    vals[[i_val]] <- val
  }
  vals
}

