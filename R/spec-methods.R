
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

## HAS_TESTS
#' @export
make_class_spec.BayesProj_spec_ts_dampedtrend <- function(spec) "dampedtrend"


## 'make_consts' --------------------------------------------------------------

#' Derive Values for 'consts' Argument used with TMB
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

## HAS_TESTS
#' @export
make_consts.BayesProj_spec_ts_dampedtrend <- function(spec, y) {
    mult <- 0.05
    mult_init <- 10 
    scale_sd_y <- spec$scale_sd_y
    scale_sd_level <- spec$scale_sd_level
    scale_sd_trend <- spec$scale_sd_trend
    damp_min <- spec$damp_min
    damp_max <- spec$damp_max
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
    

## 'make_hyper' ---------------------------------------------------------------

#' Derive Values for 'hyper' Argument used with TMB
#'
#' @param spec An object of class "BayesProj_spec_ts"
#' @param y A vector containing observed values
#' for the variable being modelled. 
#'
#' @returns A named numeric vector
#'
#' @noRd
make_hyper <- function(spec, y) {
    UseMethod("make_hyper")
}

## HAS_TESTS
#' @export
make_hyper.BayesProj_spec_ts_dampedtrend <- function(spec, y) {
  n_time <- length(y)
  trend <- rep.int(0, times = n_time)
  names(trend) <- paste0("time", seq_len(n_time))
  sd <- c(log_sd_y = 0,
          log_sd_level = 0,
          log_sd_trend = 0,
          logit_damp = 0)
  c(trend, sd)
}


## 'make_par' -----------------------------------------------------------------

#' Derive Values for 'par' Argument used with TMB
#'
#' @param spec An object of class "BayesProj_spec_ts"
#' @param y A vector containing observed values
#' for the variable being modelled. 
#'
#' @returns A named numeric vector
#'
#' @noRd
make_par <- function(spec, y) {
    UseMethod("make_par")
}

## HAS_TESTS
#' @export
make_par.BayesProj_spec_ts <- function(spec, y) {
  n_time <- length(y)
  ans <- rep.int(0, times = n_time)
  names(ans) <- paste0("time", seq_len(n_time))
  ans
}


## 'print' --------------------------------------------------------------------

## TODO - Rewrite this as formulas
## #' @export
## print.BayesProj_spec_ts_dampedtrend <- function(x, ...) {
##     nchar_offset <- 15
##     cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
##     cat(sprintf("% *s: %s\n", nchar_offset, "scale_sd_level", x$scale_sd_level))
##     cat(sprintf("% *s: %s\n", nchar_offset, "scale_sd_trend", x$scale_sd_trend))
##     cat(sprintf("% *s: %s\n", nchar_offset, "damp_min", x$damp_min))
##     cat(sprintf("% *s: %s\n", nchar_offset, "damp_max", x$damp_max))
## }






    
