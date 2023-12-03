
## 'augment' ------------------------------------------------------------------

#' @importFrom generics augment
#' @export
generics::augment

#' @export
augment.BayesProj_fitted <- function(x,
                                     interval = 0.95,
                                     ...) {
  data <- object$data
  smoothed <- components(object = x,
                         what = "smoothed",
                         interval = interval)
  merge(data, smoothed, all.x = TRUE, sort = FALSE)
}



## 'components' ---------------------------------------------------------------

#' @importFrom generics components
#' @export
generics::components

## HAS_TESTS
#' Extract Components from a Model Object
#'
#' @param object A results object.
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
#' @export
components.BayesProj_fitted <- function(object,
                                        what = NULL,
                                        interval = 0.95,
                                        ...) {
  draws_post <- draws_vals_fit(object)
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
                measurevar = ".value",
                interval = interval)
  if (length(ans) == 1L)
    ans <- ans[[1L]]
  ans
}



