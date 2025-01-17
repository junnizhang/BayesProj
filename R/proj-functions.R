
## Functions designed for use with BayesProj-proj objects that
## aren't technically methods

## 'composite' ----------------------------------------------------------------

#' Combine Results from Estimates and Projection
#'
#' Combine estimated and projected values for demographic
#' indicator, in a format suitable for graphing
#' or analysis.
#'
#' @param x Results from a call to [project_ts()].
#' @param interval Width of credible intervals.
#' A number between 0 and 1. Default is `0.95`.
#'
#' @returns A tibble
#'
#' @seealso
#' - [project_ts()] to project a time series model
#' - \code{\link[=augment.BayesProj_proj]{augment()}}
#' combines data and benchmarks.
#' - \code{\link[=components.BayesProj_proj]{components()}}
#'   extracts forecasts for parameters and hyper-parameter.
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
#' composite(projected)
#' @export
composite <- function(x, interval = 0.95) {
  checkmate::assert_class(x, classes = "BayesProj_proj")
  data <- x$data
  indvar <- x$indvar
  timevar <- x$timevar
  aug <- augment(x, interval = interval)
  has_benchmarks <- ".bench.lower" %in% names(aug)
  is_ind_list <- is.list(data[[indvar]])
  if (has_benchmarks) {
    data <- cbind(data,
                  .bench.lower = NA_real_,
                  .bench.mid = NA_real_,
                  .bench.upper = NA_real_)
  }
  if (is_ind_list) {
    probs <- make_probs(interval)
    m <- matrix(unlist(data[[indvar]]),
                nrow = nrow(data),
                byrow = TRUE)
    q <- matrixStats::rowQuantiles(m, probs = probs)
    data[[indvar]] <- q[, 2L]
    data <- cbind(data,
                  .lower = q[, 1L],
                  .upper = q[, 3L])
  }
  else {
    data <- cbind(data,
                  .lower = NA_real_,
                  .upper = NA_real_)
  }
  if (is.factor(data[[timevar]]))
    data[[timevar]] <- convert_factor_to_integer(data[[timevar]])
  data <- cbind(.variant = "Estimated", data)
  aug <- aug[-match(".probability", names(aug))]
  names(aug)[match(".fitted", names(aug))] <- indvar
  if (is.factor(aug[[timevar]]))
    aug[[timevar]] <- convert_factor_to_integer(aug[[timevar]])
  aug <- cbind(.variant = "Projected", aug)
  data <- data[names(aug)]
  ans <- rbind(data, aug)
  if (is.character(ans[[timevar]]))
    ans[[timevar]] <- as.integer(ans[[timevar]])
  is_final <- ans[[timevar]] == max(data[[timevar]])
  if (!is_ind_list) {
    ans[[".lower"]][is_final] <- ans[[indvar]][is_final]
    ans[[".upper"]][is_final] <- ans[[indvar]][is_final]
  }
  if (has_benchmarks) {
    ans[[".bench.lower"]][is_final] <- ans[[indvar]][is_final]
    ans[[".bench.mid"]][is_final] <- ans[[indvar]][is_final]
    ans[[".bench.upper"]][is_final] <- ans[[indvar]][is_final]
  }    
  ans <- tibble::as_tibble(ans)
  ans
}

