
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'make_class_spec' ----------------------------------------------------------

#' Make 'class_spec' String
#'
#' Make class descriptor used within TMB
#' 
#' @param spec An object of class "BayesProj_spec"
#'
#' @returns A string
#'
#' @noRd
make_class_spec <- function(spec) {
    UseMethod("make_class_spec")
}

#' @export
make_class_spec.BayesProj_spec_ts_dampedtrend <- function(spec) "damped_trend"


## 'make_consts' --------------------------------------------------------------

#' Derive Values for 'constant' Argument used with TMB
#'
#' @param spec An object of class "BayesProj_spec_ts"
#' @param y A vector containing observed values
#' for the variable being modelled. May contain NAs
#' (though guaranteed to have some non-NAs.)
#'
#' @returns A named numeric vector
#'
#' @noRd
make_consts <- function(spec, y) {
    UseMethod("make_consts")
}

#' @export
make_consts.BayesRates_spec_ts_damptrend <- function(spec, y) {
    mult <- 0.05
    mult_init <- 10 
    scale_sd_y <- spec$scale_sd_y
    scale_sd_level <- spec$scale_sd_level
    scale_sd_trend <- spec$scale_sd_trend
    y_mean <- mean(y, na.rm = TRUE)
    sd_level_init <- mult_init * y_mean  ## proper but non-informative
    sd_trend_init <- y_mean              ## weakly informative - only ruling out craziest values
    if (is.null(scale_sd_y))
        scale_sd_y <- mult * y_mean
    if (is.null(scale_sd_level))
        scale_sd_level <- mult * y_mean
    if (is.null(scale_sd_trend))
        scale_sd_trend <- mult * y_mean
    c(sd_level_init = sd_level_init,
      sd_trend_init = sd_trend_init,
      scale_sd_y = scale_sd_y,
      scale_sd_level = scale_sd_level,
      scale_sd_trend = scale_sd_trend,
      damp_min = damp_min,
      damp_max = damp_max)
}
    


## 'print' --------------------------------------------------------------------

## TODO - Rewrite this as formulas
## #' @export
## print.BayesRates_spec_ts_damptrend <- function(x, ...) {
##     nchar_offset <- 15
##     cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
##     cat(sprintf("% *s: %s\n", nchar_offset, "scale_sd_level", x$scale_sd_level))
##     cat(sprintf("% *s: %s\n", nchar_offset, "scale_sd_trend", x$scale_sd_trend))
##     cat(sprintf("% *s: %s\n", nchar_offset, "damp_min", x$damp_min))
##     cat(sprintf("% *s: %s\n", nchar_offset, "damp_max", x$damp_max))
## }






    
