
## 'augment' ------------------------------------------------------------------

#' Projection of Indicator, Combined with Benchmarks
#'
#' Given output from a call to [project_ts()],
#' extract forecasts for the demographic indicator,
#' merged with the benchmarks (if used).
#'
#' The values for the indicator, and for the benchmarks
#' are shown in the original units, even if
#' `log = TRUE` in the original call to
#' [fit_ts()].
#'
#' @param x Results from a call to [project_ts()].
#' @param interval Width of credible intervals.
#' A number between 0 and 1. Default is `0.95`.
#' @param ... Not currently used.
#'
#' @returns A [tibble][tibble::tibble-package],
#' containing the 'by' variables from the historical estimates,
#' the time variable, and the following variables:
#' - `.fitted`: Point estimates (posterior medians) of rates.
#' - `.lower`, `.upper`: Lower and upper bounds of
#' credible intervals specified by `interval`.
#' - `.probability`: A list column with all draws
#' from the posterior distribution.
#'
#' If a value for `spec_bench` was provided in the call
#' to [project_ts()], then information on the benchmarks
#' is also included.
#'
#' @seealso
#' - [fit_ts()] to fit a time series model to historical data.
#' - [project_ts()] to forecast a time series model.
#' - [Benchmarks()] to specify "benchmarks" (expert judgements
#'   about plausible future values) for a forcast.
#' - \code{\link[=components.BayesProj_proj]{components()}}
#'   extracts forecasts for parameters and hyper-parameter.
#' - \code{\link[=n_draw<-]{n_draw()}} sets the default number
#' of draws from the posterior distribution.
#' - [composite()] combines estimates and projections
#'   in a single tibble.
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
#'                              2030,  1.8,  2.3))
#' projected <- project_ts(fitted = fitted,
#'                         time_labels = 2023:2030,
#'                         spec_bench = bench)
#' augment(projected)
#' @export
augment.BayesProj_proj <- function(x,
                                   interval = 0.95,
                                   ...) {
  data <- x$data
  byvar <- x$byvar
  timevar <- x$timevar
  indvar <- x$indvar
  log <- x$log
  comp <- components(object = x,
                     interval = interval)
  ans <- comp[[indvar]]
  if (log) {
    for (nm in c(".fitted", ".lower", ".upper"))
      ans[[nm]] <- exp(ans[[nm]])
    ans[[".probability"]] <- lapply(ans[[".probability"]], exp)
  }
  has_benchmarks <- "benchmarks" %in% names(comp)
  if (has_benchmarks) {
    benchmarks <- comp[["benchmarks"]]
    benchmarks <- convert_bench_to_interval(benchmarks = benchmarks,
                                            interval = interval,
                                            log = log)
    ans <- merge(benchmarks, ans,
                 by = c(byvar, timevar),
                 sort = FALSE)
  }
  ans <- tibble::as_tibble(ans)
  ans
}


## 'components' ---------------------------------------------------------------

#' Components from a Projection
#'
#' If the `log` argument to
#' [fit_ts()] was `TRUE`, then all projected
#' values are reported on the log scale.
#'
#' @param object Results from a call to [project_ts()].
#' @param what Components to be extracted.
#' If no value supplied, all available components
#' are extracted.
#' @param interval Width of credible intervals.
#' A number between 0 and 1. Default is `0.95`.
#' @param ... Not currently used.
#'
#' @returns A named list of tibbles, or, if `what`
#' is a single value, a single tibble.
#' Each tibble contains
#'
#' - classification variables, if present
#' - `.fitted` Point estimates (posterior medians).
#' - `.lower`, `.upper` Lower and upper bounds of
#' credible intervals.
#' - `.probability`. A list column with all draws
#' from the posterior distribution.
#'
#' @seealso
#' - [project_ts()] to project a time series model
#' - \code{\link[=augment.BayesProj_proj]{augment()}}
#' combines data and benchmarks.
#' - [composite()] combines estimates and projections
#'   in a single tibble.
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
#' components(projected)
#' @export
components.BayesProj_proj <- function(object,
                                      what = NULL,
                                      interval = 0.95,
                                      ...) {
  ans <- draw_post_proj(object)
  ans <- lapply(ans,
                make_credible_intervals,
                interval = interval)
  benchmarks_df <- make_benchmarks_df(object)
  if (!is.null(benchmarks_df))
    ans <- c(ans, list(benchmarks = benchmarks_df))
  if (!is.null(what)) {
    nms <- names(ans)
    if (!all(what %in% nms)) {
      stop(gettextf("Invalid value for '%s' : valid choices are %s",
                    "what",
                    paste(sprintf("'%s'", nms), collapse = ", ")),
           call. = FALSE)
    }
    ans <- ans[what]
  }
  ans <- lapply(ans, tibble::as_tibble)
  if (length(ans) == 1L)
    ans <- ans[[1L]]
  ans
}


## 'n_draw' -------------------------------------------------------------------

#' @export
n_draw.BayesProj_proj <- function(x) {
  x$n_draw
}

## HAS_TESTS
#' @export
`n_draw<-.BayesProj_proj` <- function(x, value) {
  value <- checkmate::assert_count(value,
                                   positive = TRUE,
                                   coerce = TRUE)
  data <- x$data
  indvar <- x$indvar
  ind <- data[[indvar]]
  ind_is_list <- is.list(ind)
  if (ind_is_list) {
    n_draw_ind <- length(ind[[1L]])
    if (value < n_draw_ind) {
      ind <- matrix(unlist(ind), nrow = n_draw_ind, ncol = length(ind))
      ind <- ind[seq_len(value), , drop = FALSE]
      ind <- apply(ind, 2L, identity, simplify = FALSE)
      x$data[[indvar]] <- ind
      x$n_draw <- value
    }
    else if (value == n_draw_ind)
      x$n_draw <- value
    else if (value > n_draw_ind)
      stop(gettextf("replacement value for '%s' [%d] greater than number of draws in inputs [%d]",
                    "n_draw",
                    value,
                    n_draw_ind),
           call. = FALSE)
  }
  else
    x$n_draw <- value
  x
}


## 'print' --------------------------------------------------------------------

#' @export
print.BayesProj_proj <- function(x, ...) {
  ## extract info
  spec_ts <- x$spec_ts
  timevar <- x$timevar
  byvar <- x$byvar
  n_draw <- x$n_draw
  has_byvar <- length(byvar) > 0L
  ## make strings
  str_title <- sprintf("--- Object of class \"%s\" ---\n",
                       class(x)[[1L]])
  str_timevar <- sprintf("  timevar: %s\n", timevar)
  val_byvar <- if (has_byvar) paste(byvar, collapse = ", ") else "<none>"
  str_byvar <- sprintf("    byvar: %s\n", val_byvar)
  str_n_draw <- sprintf("   n_draw: %s\n", n_draw)
  ## print
  cat(str_title)
  cat("\n")
  cat(str_timevar)
  cat(str_byvar)
  cat(str_n_draw)
  ## return
  invisible(x)
}
