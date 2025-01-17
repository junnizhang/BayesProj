
## User-visible constructors --------------------------------------------------

#' Specify Benchmarks
#'
#' Specify "benchmarks" for a projection, ie judgments
#' about the likely range of future values. The benchmarks
#' are typically passed to function
#' [project_ts()].
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
#' @section Merging benchmarks:
#' 
#' Function [project_ts()] combines benchmarks from
#' `Benchmarks()` with fitted values from [fit_ts()]
#' to make a projection. The combining includes
#' merging on any 'by' variables in the benchmarks
#' or fitted values.
#'
#' @section Interpolating and extrapolating benchmarks:
#'
#' Benchmarks do not need to be specified for every
#' period in the projection interval. Function
#' [project_ts()] interpolates or extrapolates
#' benchmarks for any periods where they are
#' not specified. The intepolation and extrapolation
#' is done using function [stats::splinefun()].
#' The type of spline is controlled using the
#' `method` argument, which is passed to 
#' [stats::splinefun()]. The default value for
#' `method` is `"natural"` (which is different from
#' the default for [stats::splinefun()]).
#'
#' Methods `"monoH.FC"` and `"hyman"` produce
#' monotonic splines.
#'
#' Extrapolation should be done with care. In general,
#' it is safest to explicitly set benchmarks for the
#' final period of the projection. See the documention
#' for [stats::splinefun()] for advice on appropriate
#' methods for extrapolation.
#' 
#' @param data Data frame with a time variable,
#' variables `q50` and `q90`, and possibly one or
#' more 'by' variables. See below for
#' details.
#' @param method Method for interpolating or
#' extrapolating benchmarks, if benchmarks
#' are not provided for all future years.
#' Passed to function [stats::splinefun()].
#' Default is `"natural"`.
#'
#' @returns An object of class `"BayesProj_spec_bench"`.
#'
#' @seealso
#' - The object created a call to `Benchmarks()`
#'   is normally supplied to function `project_ts()`.
#' - The benchmarks are interpolated and extrapolated
#'   using function [stats::splinefun()].
#'
#' @examples
#' df <- data.frame(time = 2030,
#'                  q50 = 1.7,
#'                  q90 = 2.1)
#' Benchmarks(df)
#'
#' df2 <- data.frame(time = c(2030, 2030),
#'                   area = c("Urban", "Rural"),
#'                   q50 = c(1.5, 1.8),
#'                   q90 = c(2, 2.1))
#' Benchmarks(df2)
#' @export
Benchmarks <- function(data,
                       method = c("natural",
                                  "fmm",
                                  "periodic",
                                  "monoH.FC",
                                  "hyman")) {
  check_data_benchmarks(data)
  method <- match.arg(method)
  new_BayesProj_spec_bench(data = data,
                           method_spline = method)
}


## Internal constructors ------------------------------------------------------

#' New Object of Class "BayesProj_spec_bench"
#'
#' @inheritParams Benchmarks
#' @param method_spline String. 'method' argument
#' for function [stats::splinefun()].
#' 
#' @return Object of class "BayesProj_spec_bench"
#'
#' @noRd
new_BayesProj_spec_bench <- function(data, method_spline) {
  ans <- list(data = data,
              method_spline = method_spline)
  class(ans) <- "BayesProj_spec_bench"
  ans
}

