
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
#' the results from a call to [fit_ts()].
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
#' bench <- Benchmarks(tribble(~time, ~q50, ~q90,
#'                             2030,  1.8,  2.3))
#' projected <- project_ts(fitted = fitted,
#'                         time_labels = 2023:2030,
#'                         spec_bench = bench)
#' projected
#' @export
project_ts <- function(fitted, time_labels, spec_bench = NULL) {
  check_is_fitted(fitted)
  data <- fitted$data
  timevar <- fitted$timevar
  byvar <- fitted$byvar
  indvar <- fitted$indvar
  spec_ts <- fitted$spec_ts
  by <- fitted$by
  log <- fitted$log
  n_draw <- fitted$n_draw
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
    method_spline <- spec_bench$method_spline
    indvar_final <- get_indvar_final(data = data,
                                     indvar = indvar,
                                     timevar = timevar,
                                     byvar = byvar,
                                     n_draw = n_draw)
    benchmarks <- make_benchmarks(mean_sd_bench = mean_sd_bench,
                                  indvar_final = indvar_final,
                                  method_spline = method_spline,
                                  byvar = byvar)
  }
  else {
    benchmarks <- by
    benchmarks$.benchmarks <- rep(list(NULL), times = nrow(by))
    method_spline <- NULL
  }
  new_BayesProj_proj(data = data,
                     indvar = indvar,
                     timevar = timevar,
                     byvar = byvar,
                     spec_ts = spec_ts,
                     fitted = fitted$fitted,
                     by = by,
                     labels_time = fitted$labels_time,
                     log = log,
                     n_draw = n_draw,
                     labels_time_project = time_labels,
                     benchmarks = benchmarks,
                     method_spline = method_spline)
}
