
## User-visible constructors --------------------------------------------------

#' Specify a Damped Linear Trend Model
#'
#' Specify a "damped linear trend" time series model.
#'
#' @section Mathematical details:
#' 
#' The damped linear trend time model has the form
#'
#' \deqn{y_t = \alpha_t + \xi_t}
#' 
#' \deqn{\alpha_t = \alpha_{t-1} + \delta_{t-1} + \epsilon_t}
#'
#' \deqn{\delta_t = \phi \delta_{t-1} + \varepsilon_t}
#'
#' where
#' - \eqn{y_t} is observed  values,
#' - \eqn{\alpha_t} is a 'level' term,
#' - \eqn{\delta_t} is a 'trend' term,
#' - \eqn{\phi} controls the amount of damping, and
#' - \eqn{\xi_t}, \eqn{\epsilon_t}, and \eqn{\varepsilon_} are all
#'    normally-distributed 'error' or 'innovation' terms.
#'
#' Damping parameter \eqn{\phi} is restricted to the interval
#'
#' \deqn{\phi_{\text{min}} < \phi < \phi_{\text{max}}}
#'
#' where \eqn{\phi_{\text{min}}} is argument `damp_min` and
#' \eqn{\phi_{\text{max}}} is argument `damp_max`.
#'
#' The standard devations for \eqn{\xi_t},
#' \eqn{\epsilon_t}, and \eqn{\varepsilon}
#' have half normal priors. Each of these
#' priors has an associated scale parameter.
#'
#' @param scale_obs Scale parameter governing
#' expected size of observation errors.
#' @param scale_level Scale parameter governing
#' expected size of errors for level term.
#' @param scale_trend Scale parameter governing
#' expected size of errors for trend term.
#' @param damp_min,damp_max Minimum and maximum values
#' for damping parameter. Defaults are `0.8` and
#' `0.98`.
#'
#' @returns An object of class `"BayesProj_spec_dampedtrend"`.
#'
#' @examples
#' DampedTrend()
#'
#' ## allow for more rapid change
#' DampedTrend(scale_level = 0.5,
#'             scale_trend = 0.5)
#'
#' ## tighter prior on amount of damping
#' DampedTrend(damp_min = 0.9,
#'             damp_max = 0.95)
#' @export
DampedTrend <- function(sale_obs = NULL,
                        scale_level = NULL,
                        scale_trend = NULL,
                        damp_min = 0.8,
                        damp_max = 0.98) {
    checkmate::assert_number(scale_obs, lower = 0, finite = TRUE, null.ok = TRUE)
    checkmate::assert_number(scale_level, lower = 0, finite = TRUE, null.ok = TRUE)
    checkmate::assert_number(scale_trend, lower = 0, finite = TRUE, null.ok = TRUE)
    check_gt_zero(scale_obs, nm = "scale_obs")
    check_gt_zero(scale_level, nm = "scale_level")
    check_gt_zero(scale_trend, nm = "scale_trend")
    checkmate::assert_number(damp_min, lower = 0, upper = 1)
    checkmate::assert_number(damp_max, lower = 0, upper = 1)
    if (damp_min > damp_max) {
        stop(gettextf("'%s' is greater than '%s'",
                      "damp_min",
                      "damp_max"),
             call. = FALSE)
    }
    new_BayesProj_spec_ts_dampedtrend(scale_sd_y = scale_obs,
                                      scale_sd_level = scale_level,
                                      scale_sd_trend = scale_trend,
                                      damp_min = damp_min,
                                      damp_max = damp_max)
}
    


## Internal constructors ------------------------------------------------------

#' New Object of Class "BayesProj_spec_ts_dampedtrend"
#'
#' @param scale_obs Scale parameter for half-normal
#' prior for standard deviation for observation errors.
#' @param scale_level Scale parameter for half-normal
#' prior for standard deviation for innovations
#' in level term.
#' @param scale_trend Scale parameter for half-normal
#' prior for standard deviation for innovations
#' in level term.
#' 
#' @return Object of class "BayesProj_spec_ts_damped_trend"
#'
#' @noRd
new_BayesProj_spec_ts_dampedtrend <- function(scale_obs,
                                              scale_level,
                                              scale_trend,
                                              damp_min,
                                              damp_max) {
    ans <- list(scale_obs = scale_obs,
                scale_level = scale_level,
                scale_trend = scale_trend,
                damp_min = damp_min,
                damp_max = damp_max)
    class(ans) <- c("BayesProj_spec_ts_dampedtrend", "BayesProj_spec_ts")
    ans
}

