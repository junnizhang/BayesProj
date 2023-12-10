
## 'augment' ------------------------------------------------------------------

#' Forecasts of Indicator, Combined with Benchmarks
#'
#' Given output from a call to [project_ts()],
#' extract forecasts for the demographic indicator,
#' merged with the benchmarks (if used).
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
#' augment(projected)
#' @export
augment.BayesProj_proj <- function(x,
                                   interval = 0.95,
                                   ...) {
  data <- x$data
  byvar <- x$byvar
  timevar <- x$timevar
  log <- x$log
  forecasted <- components(object = x,
                           what = "level",
                           interval = interval)
  if (log) {
    for (nm in c(".fitted", ".lower", ".upper"))
      forecasted[[nm]] <- exp(forecasted[[nm]])
    forecasted[[".probability"]] <- lapply(forecasted[[".probability"]], exp)
  }
  ## by <- intersect(names(data), c(timevar, byvar)) ## keeps original order
  ## ans <- merge(data, modelled, all.x = TRUE, by = by, sort = FALSE)
  ## ans <- tibble::tibble(ans)
  ans
}


## 'components' ---------------------------------------------------------------

#' Components from a Forecasted Time Series Model
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
#' - \code{\link[=augment.BayesProj_fitted]{augment()}}
#' combines data and estimates.
#' - \code{\link[=n_draw<-]{n_draw()}} sets the default number
#' of draws from the posterior distribution.
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
#' components(projected)
#' @export
components.BayesProj_proj <- function(object,
                                      what = NULL,
                                      interval = 0.95,
                                      ...) {
  draws_post <- draw_post_proj(object)
  if (!is.null(what)) {
    nms <- names(draws_post)
    if (!all(what %in% nms)) {
      stop(gettextf("Invalid value for '%s' : valid choices are %s",
                    "what",
                    paste(sprintf("'%s'", nms), collapse = ", ")),
           call. = FALSE)
    }
    draws_post <- draws_post[what]
  }
  ans <- lapply(draws_post,
                make_credible_intervals,
                interval = interval)
  if (length(ans) == 1L)
    ans <- ans[[1L]]
  ans
}


## 'n_draw' -------------------------------------------------------------------

#' Set Number of Draws from Posterior Distribution
#'
#' Specify the number of draws from the posterior
#' distribution that are generated by a results object from
#' [fit_ts()].
#'
#' @param x An object of class `"BayesProj_fitted"`,
#' generated by a call to [fit_ts()].
#' @param value Number of posterior draws. A non-negative
#' integer.
#'
#' @returns A modified version of `x`.
#'
#' @seealso
#' - [fit_ts()] fits a time series model to historical data.
#' - \code{\link[=augment.BayesProj_fitted]{augment()}}
#' combines data and estimates.
#' - \code{\link[=components.BayesProj_fitted]{components()}}
#' extracts estimates of parameters and hyper-parameters.
#'
#' @examples
#' library(tibble)
#' data <- tribble(~time, ~val,
#'                 2015,  4.3,
#'                 2016,  4.1,
#'                 2020,  3.5,
#'                 2022,  3.6)
#' fit <- fit_ts(data, indvar = "val")
#' fit
#' n_draw(fit) <- 10
#' fit
#' @export
`n_draw<-` <- function(x, value) {
  UseMethod("n_draw<-")
}

## NO_TESTS
#' @export
`n_draw<-.BayesProj_proj` <- function(x, value) {
  value <- checkmate::assert_count(value,
                                   positive = TRUE,
                                   coerce = TRUE)
  x$n_draw <- value
  x
}



## 'print' --------------------------------------------------------------------

## #' @export
## print.BayesProj_proj <- function(x, ...) {
##   ## extract info
##   spec_ts <- x$spec_ts
##   timevar <- x$timevar
##   byvar <- x$byvar
##   n_draw <- x$n_draw
##   has_byvar <- length(byvar) > 0L
##   ## make strings
##   str_title <- sprintf("--- Object of class \"%s\" ---\n",
##                        class(x)[[1L]])
##   str_timevar <- sprintf("  timevar: %s\n", timevar)
##   val_byvar <- if (has_byvar) paste(byvar, collapse = ", ") else "<none>"
##   str_byvar <- sprintf("    byvar: %s\n", val_byvar)
##   str_n_draw <- sprintf("   n_draw: %s\n", n_draw)
##   ## print
##   cat(str_title)
##   cat("\n")
##   cat(str_timevar)
##   cat(str_byvar)
##   cat(str_n_draw)
##   ## return
##   invisible(x)
## }
