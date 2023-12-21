
#' Fit a Time Series Model
#'
#' Fit a time series model to a demographic
#' indicator, such as the total fertility rate.
#'
#' @section Format of indicator:
#'
#' The demographic indicator can be a single set of estimates
#' or it can be multiple draws from a posterior
#' distribution. If it is a single set of estimates,
#' then `indvar` should be an ordinary numeric vector.
#' If it is multiple draws, then it should be a
#' list of vectors, each of which has the same length.
#' For an example of multiple draws, see [BayesRates::total_rate()].
#'
#' @section 'by' variable:
#'
#' `model_ts()` has an optional `byvar` argument,
#' specifying 'by' variables in `data`. If
#' a value for `byvar` is supplied, then
#' separate models are fitted for each combination
#' of the 'by' variables.
#'
#' @section Modelling on the log scale:
#'
#' If the `log` argument is `TRUE`, then the
#' model is fitted to logged values of the
#' indicator. This can be appropriate when an indicator
#' is close to zero and has more room to move
#' up than down.
#'
#' @param data Data frame containing estimates
#' for summary indicator.
#' @param indvar Name of the variable holding
#' the demographic indicator.
#' @param timevar Name of the time variable.
#' @param byvar Names of classification variables
#' in `data`. Optional.
#' @param spec_ts Specification for a time series model.
#' See [DampedTrend()].
#' @param log Whether to estimate the model on the
#' log scale. Default is `FALSE`.
#'
#' @returns An object of class `"BayesProj_fitted"`.
#'
#' @seealso
#' - [DampedTrend()] to specify a "damped linear trend"
#'   time series model.
#' - [project_ts] to project an indicator, using
#'   results from a call to `model_ts()`.
#' - [BayesRates::total_rate()] to calculate a demographic
#'   indicator.
#' - [components()] for parameters from the fitted model
#' - [augment()] for smoothed and interpolated
#'   values of the indicator
#'   from the fitted model, combined with the original data.
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
#' @export
fit_ts <- function(data,
                   indvar,
                   timevar = "time",
                   byvar = character(),
                   spec_ts = DampedTrend(),
                   log = FALSE) {
  ## check inputs
  checkmate::assert_string(indvar, min.chars = 1L)
  if (grepl("^\\.", indvar))
    stop("'indvar' starts with a '.'")
  checkmate::assert_string(timevar, min.chars = 1L)
  if (grepl("^\\.", timevar))
    stop("'timevar' starts with a '.'")
  checkmate::assert_character(byvar,
			      min.chars = 1L,
			      any.missing = FALSE,
			      unique = TRUE)
  if (any(grepl("^\\.", byvar)))
    stop("'byvar' includes a name that starts with a '.'")
  check_data(data = data,
	     indvar = indvar,
	     timevar = timevar,
	     byvar = byvar)
  if (!inherits(spec_ts, "BayesProj_spec_ts"))
    stop(gettextf("'%s' has class \"%s\"",
		  "spec_ts",
		  class(spec_ts)),
	 call. = FALSE)
  check_log(log)
  ## prepare inputs
  data <- format_timevar(data = data,
                         timevar = timevar)
  inputs <- prepare_inputs_fit(data = data,
			       indvar = indvar,
			       timevar = timevar,
			       byvar = byvar,
                               log = log)
  ## fit model
  fitted <- make_fitted(inputs = inputs,
			spec_ts = spec_ts)
  ## assemble results
  by <- inputs[["key"]]
  labels_time <- levels(data[[timevar]])
  new_BayesProj_fitted(data = data,
                       indvar = indvar,
                       timevar = timevar,
                       byvar = byvar,
                       spec_ts = spec_ts,
                       fitted = fitted,
                       by = by,
                       labels_time = labels_time,
                       log = log)
}

















