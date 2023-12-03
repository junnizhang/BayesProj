
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'draw_post_fit_one' --------------------------------------------------------

#' Draw from the Posterior Distribution for Parameters
#' - for One Fitted Model
#'
#' @param spec An object of class "BayesProj_spec_ts"
#' @param mean Named list of numeric vectors
#' @param prec Matrix
#' @param n_draw Number of draws
#'
#' @returns A matrix with 'n_draw' columns
#'
#' @noRd
draw_post_fit_one <- function(spec, mean, prec, n_draw) {
  UseMethod("draw_post_fit_one")
}

## HAS_TESTS
#' @export
draw_post_fit_one.BayesProj_spec_ts <- function(spec, mean, prec, n_draw) {
  damp_min <- spec$damp_min
  damp_max <- spec$damp_max
  n_period <- length(mean$level)
  mean <- unlist(mean)
  ans <- rmvn(n = n_draw,
              mean = mean,
              prec = prec)
  ans <- t(ans)
  i_sd <- (2L * n_period) + (1:3)
  i_damp <- (2L * n_period) + 4L
  ans[i_sd, ] <- exp(ans[i_sd, ])
  ans[i_damp, ] <- damp_min + (damp_max - damp_min) * invlogit(ans[i_damp, ])
  ans
}


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
  y_mean_abs <- mean(abs(y), na.rm = TRUE)
  sd_level_init <- mult_init * y_mean_abs  ## proper but non-informative
  sd_trend_init <- y_mean_abs              ## weakly informative - only ruling out craziest values
  if (is.null(scale_sd_y))
    scale_sd_y <- mult * y_mean_abs
  if (is.null(scale_sd_level))
    scale_sd_level <- mult * y_mean_abs
  if (is.null(scale_sd_trend))
    scale_sd_trend <- mult * y_mean_abs
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
#' for the variable being modelled. 
#'
#' @returns A named numeric vector
#'
#' @noRd
make_hyper <- function(spec) {
  UseMethod("make_hyper")
}

## HAS_TESTS
#' @export
make_hyper.BayesProj_spec_ts_dampedtrend <- function(spec) {
  c(log_sd_y = 0,
    log_sd_level = 0,
    log_sd_trend = 0,
    logit_damp = 0)
}


## 'make_labels_fit' ----------------------------------------------------------

#' Derive Labels to Use When Reporting on Fitted Model
#'
#' Creates labels for a single combination of
#' the 'by' variables
#'
#' @param spec Object of class 'BayesProj_spec_ts'
#' @param labels_time Vector of labels for historical periods,
#' including periods with no data
#'
#' @returns A character vector
#'
#' noRd
make_labels_fit <- function(spec, labels_time) {
  UseMethod("make_labels_fit")
}

## HAS_TESTS
#' @export
make_labels_fit.BayesProj_spec_ts_dampedtrend <- function(spec, labels_time) {
  level <- labels_time
  trend <- labels_time
  hyper <- c("sd_obs", "sd_level", "sd_trend", "damp")
  c(level, trend, hyper)
}  


## 'make_level' ---------------------------------------------------------------

#' Derive Values for 'level' Argument used with TMB
#'
#' @param spec An object of class "BayesProj_spec_ts"
#' @param y A vector containing observed values
#' for the variable being modelled. 
#'
#' @returns A named numeric vector
#'
#' @noRd
make_level <- function(spec, y) {
  UseMethod("make_level")
}

## HAS_TESTS
#' @export
make_level.BayesProj_spec_ts <- function(spec, y) {
  n_time <- length(y)
  ans <- rep.int(0, times = n_time)
  names(ans) <- paste0("time", seq_len(n_time))
  ans
}


## 'make_parameters' ----------------------------------------------------------

#' Derive Values for 'parameters' Argument used with TMB
#'
#' @param spec An object of class "BayesProj_spec_ts"
#' @param y A vector containing observed values
#' for the variable being modelled. 
#'
#' @returns A named numeric vector
#'
#' @noRd
make_parameters <- function(spec, y) {
  UseMethod("make_parameters")
}

## HAS_TESTS
#' @export
make_parameters.BayesProj_spec_ts <- function(spec, y) {
  level <- make_level(spec = spec, y = y)
  trend <- make_trend(spec = spec, y = y)
  hyper <- make_hyper(spec = spec)
  list(level = level,
       trend = trend,
       hyper = hyper)
}


## 'make_random' --------------------------------------------------------------

#' Make 'random' Argument used with TMB
#'
#' @param spec An object of class "BayesProj_spec"
#'
#' @returns A character vector
#'
#' @noRd
make_random <- function(spec) {
  UseMethod("make_random")
}

## HAS_TESTS
#' @export
make_random.BayesProj_spec_ts_dampedtrend <- function(spec)
  c("level", "trend")


## 'make_transforms_hyper' ----------------------------------------------------

#' Functions to Transform Hyper-Parameters to Natural Units
#'
#' @param spec An object of class "BayesProj_spec_ts"
#' @param y A vector containing observed values
#' for the variable being modelled. 
#'
#' @returns A named numeric vector
#'
#' @noRd
make_trend <- function(spec) {
  UseMethod("make_trend")
}


## 'make_trend' ---------------------------------------------------------------

#' Derive Values for 'trend' Argument used with TMB
#'
#' @param spec An object of class "BayesProj_spec_ts"
#' @param y A vector containing observed values
#' for the variable being modelled. 
#'
#' @returns A named numeric vector
#'
#' @noRd
make_trend <- function(spec, y) {
  UseMethod("make_trend")
}

## HAS_TESTS
#' @export
make_trend.BayesProj_spec_ts_dampedtrend <- function(spec, y) {
  n_time <- length(y)
  ans <- rep.int(0, times = n_time)
  names(ans) <- paste0("time", seq_len(n_time))
  ans
}


## 'make_vname_fit' -----------------------------------------------------------

#' Derive Names of Variables to Use When Reporting on Fitted Model
#'
#' Creates identifiers for a single combination of
#' the 'by' variables
#'
#' @param spec Object of class 'BayesProj_spec_ts'
#' @param timevar Name of the time variable
#'
#' @returns A character vector
#'
#' noRd
make_vname_fit <- function(spec, timevar) {
  UseMethod("make_vname_fit")
}

## HAS_TESTS
#' @export
make_vname_fit.BayesProj_spec_ts_dampedtrend <- function(spec, timevar) {
  c(timevar, timevar, "hyper")
}  


## 'make_what_fit' ------------------------------------------------------------

#' Derive Identifiers for Batches of Parameters
#' to Use When Reporting on Fitted Model
#'
#' Creates identifiers for a single combination of
#' the 'by' variables
#'
#' @param spec Object of class 'BayesProj_spec_ts'
#' @param labels_time Vector of labels for historical periods,
#' including periods with no data
#'
#' @returns A character vector
#'
#' noRd
make_what_fit <- function(spec, labels_time) {
  UseMethod("make_what_fit")
}

## HAS_TESTS
#' @export
make_what_fit.BayesProj_spec_ts_dampedtrend <- function(spec, labels_time) {
  n_period <- length(labels_time)
  level <- rep("level", times = n_period)
  trend <- rep("trend", times = n_period)
  hyper <- rep("hyper", times = 4L)
  ans <- c(level, trend, hyper)
  ans <- factor(ans, levels = unique(ans))
  ans
}  
