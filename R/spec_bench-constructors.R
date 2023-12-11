
## User-visible constructors --------------------------------------------------

#' Specify Benchmarks
#'
#' Specify "benchmarks" for a projection, ie judgements
#' about the likely range of future values.
#'
#' The benchmarks are typically supplied to function
#' [project_ts()].
#' 
#'
#' @section Format of `data`:
#'
#' The `data` argument must include the following variables.
#'
#' - A time variable. The variable name should match the `timevar`
#'   argument used with [fit_ts()]. The labels must match, or be
#'   a subset of, the labels used in [project_ts()]. 
#' - `q50`. The 50th quantile (ie the median)
#'   for the value being forecast.
#'   There is a 50% chance that the true value will
#'   be lower than `q50`, and a 50% chance that it
#'   will be higher.
#' - `q90`. The 90th percentile for value being forecast.
#'   There is a 90% chance that the true value will
#'   be lower than `q90`, and a 10% chance that it
#'   will be higher.
#'
#' The `data` argument can, in addition, include 'by'
#' variables identifying sub-populations. These variables
#' must match, or be a subset of, the variables used with
#' [fit_ts()].
#'
#' @section Processing of benchmarks:
#'
#' Turn into mean and SD of distn on original or log scale.
#' Time variables and by variables merged with variables
#' in `data` argument from [fit_ts()].
#'
#' @param data Data frame with a time variable,
#' variables `q50` and `q90`, and possibly one or
#' more 'by' variables. See below for
#' details.
#'
#' @returns An object of class `"BayesProj_spec_bench"`.
#'
#' @seealso
#' - The object created a call to `Benchmarks()`
#'   is normally supplied to function `project_ts()`.
#'
#' @export
Benchmarks <- function(data) {
  check_data_benchmarks(data)
  new_BayesProj_spec_bench(data)
}


## Internal constructors ------------------------------------------------------

#' New Object of Class "BayesProj_spec_bench"
#'
#' @inheritParams Benchmarks
#' 
#' @return Object of class "BayesProj_spec_bench"
#'
#' @noRd
new_BayesProj_spec_bench <- function(data) {
  ans <- list(data = data)
  class(ans) <- "BayesProj_spec_bench"
  ans
}

