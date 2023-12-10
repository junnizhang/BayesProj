
#' Project Demographic Indictators, using Bayesian Methods
#'
#' Use Bayesian methods to project demographic indicators,
#' possibly informed by expert judgement.
#'
#' A typical workflow:
#'
#' 1. Use [DampedTrend()] or [AR1()] to
#'    specify a time series model.
#' 2. Use [fit_ts()] to fit the model
#'    model to historical estimates for a
#'    demographic indicator.
#' 3. Use [Benchmarks()] to capture expert judgements
#'    about likely future values for the indicator.
#' 4. Supply the outputs from [fit_ts()] and [Benchmarks()]
#'    to [project_ts()], to project future values
#'    for the indicator.
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom tibble tibble
#' @useDynLib BayesProj, .registration = TRUE
## usethis namespace: end
NULL
