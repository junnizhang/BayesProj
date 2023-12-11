
## HAS_TESTS
#' Project a Demographic Indicator
#'
#' Construct a projection for a
#' demographic indicator, based on a fitted
#' model, and, optionally, on expert judgement.
#' The expert judgement is expressed through
#' benchmarks for future values.
#' 
#' @param fitted A fitted time series model,
#' with class `"BayesProj_fitted"`. Typically
#' the results from a call to [fit_mod()].
#' @param time_labels Character vector of time labels.
#' @param spec_bench Specification of forecast
#' benchmarks. Typically the results from a call
#' to [Benchmarks()]. Optional.
#'
#' @returns An object of class `"BayesProj_proj"`.
#'
#' @seealso
#' - [fit_ts()] to fit a time series model.
#' - [Benchmarks()] to specify forecast benchmarks.
#' - [components()] for parameters from the projection.
#' - [augment()] for projections of the indicator,
#'   together with benchmarks (if supplied).
#'
#' @examples
#' library(tibble)
#' data <- tribble(~time, ~val,
#'                 2015,  4.3,
#'                 2016,  4.1,
#'                 2020,  3.5,
#'                 2022,  3.6)
#' fitted <- fit_ts(data, indvar = "val")
#' bench <- tribble(~time, ~q50, ~q90,
#'                  2030,  1.8,  2.3)
#' projected <- project(fitted = fitted,
#'                      time_labels = 2023:2030,
#'                      spec_bench = bench)
#' projected
#' @export
project_ts <- function(fitted, time_labels, spec_bench = NULL) {
  check_is_fitted(fitted)
  timevar <- fitted$timevar
  byvar = fitted$byvar
  spec_ts <- fitted$spec_ts
  by <- fitted$by
  log <- fitted$log
  has_bench <- !is.null(spec_bench)
  if (has_bench) {
    check_is_spec_bench(spec_bench)
    check_fitted_spec_bench_compatible(fitted = fitted,
                                       spec_bench = spec_bench)
    check_time_labels_spec_bench_compatible(time_labels = time_labels,
                                            spec_bench = spec_bench,
                                            timevar = timevar)
    mean_sd_bench <- make_mean_sd_bench(spec_bench = spec_bench,
                                        timevar = timevar,
                                        byvar = byvar,
                                        by = by,
                                        time_labels = time_labels,
                                        log = log)
    mean_bench <- mean_sd_bench$mean
    sd_bench <- mean_sd_bench$sd
  }
  else {
    n_by <- nrow(by)
    n_time <- length(time_labels)
    mean_bench <- replicate(n = n_by,
                            rep(0, times = n_time),
                            simplify = FALSE)
    sd_bench <- replicate(n = n_by,
                          rep(Inf, times = n_time),
                          simplify = FALSE)
  }
  new_BayesProj_proj(data = fitted$data,
                     indvar = fitted$indvar,
                     timevar = fitted$timevar,
                     byvar = byvar,
                     spec_ts = spec_ts,
                     fitted = fitted,
                     by = by,
                     labels_time_fitted = fitted$labels_time,
                     log = log,
                     labels_time_project = time_labels,
                     mean_bench = mean_bench,
                     sd_bench = sd_bench)
}
