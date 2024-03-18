
## 'augment' ------------------------------------------------------------------

#' @importFrom generics augment
#' @export
generics::augment

## HAS_TESTS
#' Smoothed Estimates of Indicator, Combined with Original Data
#'
#' Given the output from a call to [fit_ts()],
#' extract smoothed values for the demographic indicator,
#' merged with the original data.
#'
#' If `log` is `FALSE` in the original call to
#' [fit_ts()] (which is the default), then the
#' smoothed values are derived from the `level`
#' parameter. If `log` is `TRUE`, then the
#' smoothed values are derived from `exp(level)`.
#' 
#' @param x Results from a call to [fit_ts()].
#' @param interval Width of credible intervals.
#' A number between 0 and 1. Default is `0.95`.
#' @param ... Not currently used.
#'
#' @returns A [tibble][tibble::tibble-package],
#' constructed by adding four new columns to `data`:
#' - `.fitted`: Point estimates (posterior medians) of rates.
#' - `.lower`, `.upper`: Lower and upper bounds of
#' credible intervals specified by `interval`.
#' - `.probability`: A list column with all draws
#' from the posterior distribution.
#'
#' @seealso
#' - [fit_ts()] to fit a time series model.
#' - \code{\link[=components.BayesProj_fitted]{components()}}
#'   extracts parameter and hyper-parameter estimates.
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
#' fit <- fit_ts(data, indvar = "val")
#' augment(fit)
#' @export
augment.BayesProj_fitted <- function(x,
                                     interval = 0.95,
                                     ...) {
  data <- x$data
  byvar <- x$byvar
  timevar <- x$timevar
  log <- x$log
  modelled <- components(object = x,
                         what = "level",
                         interval = interval)
  if (log) {
    for (nm in c(".fitted", ".lower", ".upper"))
      modelled[[nm]] <- exp(modelled[[nm]])
    modelled[[".probability"]] <- lapply(modelled[[".probability"]], exp)
  }
  by <- intersect(names(data), c(timevar, byvar)) ## keeps original order
  ans <- merge(data, modelled, all.x = TRUE, by = by, sort = FALSE)
  ans <- tibble::tibble(ans)
  ans
}


## 'components' ---------------------------------------------------------------

#' @importFrom generics components
#' @export
generics::components

## HAS_TESTS
#' Components from a Fitted Time Series Model
#'
#' @param object Results from a call to [fit_ts()].
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
#' - [fit_ts()] to fit a time series model
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
#' fit <- fit_ts(data, indvar = "val")
#' components(fit)
#' @export
components.BayesProj_fitted <- function(object,
                                        what = NULL,
                                        interval = 0.95,
                                        ...) {
  draws_post <- draw_post_fit(object)
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

## HAS_TESTS
#' @importFrom BayesRates n_draw
#' @export
n_draw.BayesProj_fitted <- function(x, value) {
  x$n_draw
}

## HAS_TESTS
#' @importFrom BayesRates n_draw<-
#' @export
`n_draw<-.BayesProj_fitted` <- function(x, value) {
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
print.BayesProj_fitted <- function(x, ...) {
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
