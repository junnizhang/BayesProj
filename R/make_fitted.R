
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
  y1 <- val[[1L]][[1L]]
  class_spec <- make_class_spec(spec)
  par <- make_par(spec = spec, y = y1)
  hyper <- make_hyper(spec)
  parameters <- list(par = par, hyper = hyper)
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
                          silent = TRUE)
      stats::nlminb(start = f$par,
                    objective = f$fn,
                    gradient = f$gr,
                    hessian = f$hessian)
      sdreport <- TMB::sdreport(f, getReportCovariance = TRUE)
      mean <- sdreport$par.fixed
      var <- sdreport$cov.fixed
      val[[i_y]] <- list(mean = mean,
                         var = var)
    }
    vals[[i_val]] <- val
  }
  vals
}

