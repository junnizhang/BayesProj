
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


## 'get_par_final' ------------------------------------------------------------

#' Get the Most Recent Values for Time-Varying Parameters
#'
#' @param spec_ts An object of class `"BayesProj_spec_ts"`
#' @param draws_post Named list of tibbles
#' @param timevar Name of time variable
#' @param byvar Names of by variables (possibly length 0)
#'
#' @returns Data frame
#'
#' @noRd
get_par_final <- function(spec_ts, draws_post, timevar, byvar) {
  UseMethod("get_par_final")
}

## HAS_TESTS
#' @export
get_par_final.BayesProj_spec_ts_dampedtrend <- function(spec_ts,
                                                        draws_post,
                                                        timevar,
                                                        byvar) {
  level <- draws_post$level
  trend <- draws_post$trend
  is_final_level <- level[[timevar]] == max(level[[timevar]])
  is_final_trend <- trend[[timevar]] == max(trend[[timevar]])
  level <- level[is_final_level, c(byvar, ".probability"), drop = FALSE]
  trend <- trend[is_final_trend, c(byvar, ".probability"), drop = FALSE]
  ans <- merge(level, trend, by = byvar)
  n <- length(ans)
  c_level_trend <- function(level, trend)
    .mapply(c,
            dots = list(level = level, trend = trend),
            MoreArgs = list())
  par_final <- .mapply(c_level_trend,
                       dots = list(level = ans[[n - 1L]],
                                   trend = ans[[n]]),
                       MoreArgs = list())
  ans <- ans[seq_len(n - 2L)]
  ans$.par_final <- par_final
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


## 'make_labels_proj' ---------------------------------------------------------

#' Derive Labels to Use When Reporting on Projection
#'
#' Creates labels for a single combination of
#' the 'by' variables
#'
#' @param spec Object of class 'BayesProj_spec_ts'
#' @param labels_time_project Vector of labels for projection periods
#'
#' @returns A character vector
#'
#' noRd
make_labels_proj <- function(spec, labels_time_project) {
  UseMethod("make_labels_proj")
}

## HAS_TESTS
#' @export
make_labels_proj.BayesProj_spec_ts_dampedtrend <- function(spec, labels_time_project) {
  y <- labels_time_project
  level <- labels_time_project
  trend <- labels_time_project
  hyper <- c("sd_obs", "sd_level", "sd_trend", "damp")
  c(y, level, trend, hyper)
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


## 'make_mean_proj' -----------------------------------------------------------

#' Mean Vector for Benchmarked Projections
#'
#' Make the precision matrix to use for drawing from
#' the posterior distribution for benchmarked projections.
#' Unbenchmarked projections are a special case where
#' all elements of `sd_bench` are equal to `Inf`.
#'
#' @param spec Object of class `"BayeProj_spec_ts"`.
#' @param par_final Estimated values for final
#' historical period. Contents depend on class of `spec`.
#' @param hyper Numeric vector with hyper-parameters.
#' Contents depend on class of `spec`.
#' @param mean_bench Numeric vector with one mean value
#' for each period in the projection.
#' @param sd_bench Numeric vector with one standard deviation
#' for each period in the projection.
#' @param prec_proj Precision matrix
#'
#' @returns Numeric vector with length equal to 
#' three times the number of projected periods.
#'
#' @noRd
make_mean_proj <- function(spec, par_final, hyper, mean_bench, sd_bench, prec_proj) {
  UseMethod("make_mean_proj")
}

#' @export
make_mean_proj.BayesProj_spec_ts_dampedtrend <- function(spec,
                                                         par_final,
                                                         hyper,
                                                         mean_bench,
                                                         sd_bench,
                                                         prec_proj) {
  ## extract and transform parameter estimates
  level_final <- par_final[[1L]]
  trend_final <- par_final[[2L]]
  sd_y <- hyper[[1L]]
  sd_level <- hyper[[2L]]
  sd_trend <- hyper[[3L]]
  damp <- hyper[[4L]]
  n <- length(mean_bench)
  prec_y <- 1 / sd_y^2            
  prec_level <- 1 / sd_level^2    
  prec_trend <- 1 / sd_trend^2    
  prec_bench <- 1 / sd_bench^2 ## vector
  ## make vectors
  v_y <- mean_bench * prec_bench
  v_level <- c((level_final + trend_final) * prec_level,
               rep.int(0, times = n - 1L))
  v_trend <- c(damp * trend_final * prec_trend,
               rep.int(0, times = n - 1L))
  ## combine and return
  v <- c(v_y, v_level, v_trend)
  ans <- solve(prec_proj, v)
  ans <- as.numeric(ans)
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


## 'make_prec_proj' -----------------------------------------------------

## NO_TESTS
#' Precision Matrix for Benchmarked Projections
#'
#' Make the precision matrix for a draw from the
#' posterior distribution for benchmarked projections.
#' Unbenchmarked projections are a special case where
#' all elements of `sd_bench` are equal to `Inf`.
#'
#' @param spec Object of class `"BayeProj_spec_ts"`.
#' @param hyper Numeric vector with one draw from the
#' posterior distribution for the hyper-parameters.
#' Contents depend on class of `spec`.
#' @param sd_bench Numeric vector with one
#' standard deviation for each period
#' in the projection.
#'
#' @returns Sparse square matrix with number of rows
#' equal to three times the number of projected periods.
#'
#' @noRd
make_prec_proj <- function(spec, hyper, sd_bench) {
  UseMethod("make_prec_proj")
}

#' @export
make_prec_proj.BayesProj_spec_ts_dampedtrend <- function(spec, hyper, sd_bench) {
  ## extract and transform parameter estimates
  sd_y <- hyper[[1L]]
  sd_level <- hyper[[2L]]
  sd_trend <- hyper[[3L]]
  damp <- hyper[[4L]]
  n <- length(sd_bench)
  prec_y <- 1 / sd_y^2            
  prec_level <- 1 / sd_level^2    
  prec_trend <- 1 / sd_trend^2    
  prec_bench <- 1 / sd_bench^2 ## vector
  ## make submatrices
  m_y_trend <- matrix(0, nrow = n, ncol = n)
  is_offdiag <- abs(row(m_y_trend) - col(m_y_trend)) == 1L
  m_y_y <- diag(prec_y + prec_bench)
  m_level_level <- diag(c(rep(2 * prec_level + prec_y, times = n - 1L),
                          prec_level + prec_y))
  m_level_level[is_offdiag] <- -1 * prec_level
  m_trend_trend <- diag(c(rep(prec_trend * (1 + damp^2) + prec_level, times = n - 1L),
                          prec_trend))
  m_trend_trend[is_offdiag] <- -1 * prec_trend * damp
  m_y_level <- diag(rep(-1 * prec_y, times = n))
  m_level_trend <-  diag(c(rep(prec_level, times = n - 1L),
                           0))
  is_offdiag_half <- row(m_level_trend) - col(m_level_trend) == 1L
  m_level_trend[is_offdiag_half] <- -1 * prec_level
  ## combine
  rbind(cbind(m_y_y,  m_y_level,  m_y_trend),
        cbind(m_y_level, m_level_level, m_level_trend),
        cbind(m_y_trend, t(m_level_trend), m_trend_trend))
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


## 'make_vname_proj' ----------------------------------------------------------

#' Derive Names of Variables to Use When Reporting on Projection
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
make_vname_proj <- function(spec, timevar) {
  UseMethod("make_vname_proj")
}

## HAS_TESTS
#' @export
make_vname_proj.BayesProj_spec_ts_dampedtrend <- function(spec, timevar) {
  c(timevar, timevar, timevar, "hyper")
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


## 'make_what_proj' -----------------------------------------------------------

#' Derive Identifiers for Batches of Parameters
#' to Use When Reporting on Projection
#'
#' Creates identifiers for a single combination of
#' the 'by' variables
#'
#' @param spec Object of class 'BayesProj_spec_ts'
#' @param labels_time_project Vector of labels for
#' projection periods
#'
#' @returns A character vector
#'
#' noRd
make_what_proj <- function(spec, indvar, labels_time_project) {
  UseMethod("make_what_proj")
}

## HAS_TESTS
#' @export
make_what_proj.BayesProj_spec_ts_dampedtrend <- function(spec, indvar, labels_time_project) {
  n_period <- length(labels_time_project)
  y <- rep(indvar, times = n_period)
  level <- rep("level", times = n_period)
  trend <- rep("trend", times = n_period)
  hyper <- rep("hyper", times = 4L)
  ans <- c(y, level, trend, hyper)
  ans <- factor(ans, levels = unique(ans))
  ans
}  
