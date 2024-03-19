
## HAS_TESTS
#' Express Benchmarks as Intervals
#'
#' Convert from mean (possibly as a list column) and sd
#' to intervals (possibly list columns)
#'
#' @param benchmarks A data frame with 'by' variables and
#' columns '.mean' and '.sd'
#' @param interval Width of intervals
#' @param log Whether model fitted on log scale
#'
#' @returns A data frame
#'
#' @noRd
convert_bench_to_interval <- function(benchmarks, interval, log) {
  mean <- benchmarks$.mean
  sd <- benchmarks$.sd
  z <- stats::qnorm(0.5 + 0.5 * interval)
  if (is.list(mean)) {
    mid <- transpose_list(mean)
    lower <- lapply(mid, function(x) x - z * sd)
    upper <- lapply(mid, function(x) x + z * sd)
    if (log) {
      lower <- lapply(lower, exp)
      mid <- lapply(mid, exp)
      upper <- lapply(upper, exp)
    }
    lower <- transpose_list(lower)
    mid <- transpose_list(mid)
    upper <- transpose_list(upper)
    lower <- vapply(lower, mean, 0)
    mid <- vapply(mid, mean, 0)
    upper <- vapply(upper, mean, 0)
  }
  else {
    lower <- mean - z * sd
    mid <- mean
    upper <- mean + z * sd
    if (log) {
      lower <- exp(lower)
      mid <- exp(mid)
      upper <- exp(upper)
    }
  }    
  ans <- benchmarks[-match(c(".mean", ".sd"), names(benchmarks))]
  ans$.bench.lower <- lower
  ans$.bench.mid <- mid
  ans$.bench.upper <- upper
  ans
}  


## HAS_TESTS
#' Draw from the Posterior Distribution for Parameters from
#' Fitted Model - All Combinations of 'By' Variables
#'
#' @param x Object of class "BayeProj_fitted"
#'
#' @returns A names list of data frames.
#'
#' @noRd
draw_post_fit <- function(x) {
  data <- x$data
  timevar <- x$timevar
  spec <- x$spec_ts
  fitted <- x$fitted
  by <- x$by
  labels_time <- x$labels_time
  n_draw <- x$n_draw
  seed_draws_fit <- x$seed_draws_fit
  seed_restore <- sample.int(n = .Machine$integer.max, size = 1L) ## create random seed
  set.seed(seed_draws_fit) ## set pre-determined seed
  ans <- lapply(fitted,
                draw_post_fit_by,
                spec = spec,
                n_draw = n_draw,
                timevar = timevar,
                labels_time = labels_time)
  set.seed(seed_restore) ## restore randomness
  nrows <- vapply(ans[[1L]], nrow, 1L)
  rbind_list_elements <- function(x, y)
    .mapply(rbind, dots = list(x, y), MoreArgs = list())
  ans <- Reduce(rbind_list_elements, ans)
  names(ans) <- names(nrows)
  if (ncol(by) > 0L) {
    for (i in seq_along(ans)) {
      by_rep <- vctrs::vec_rep_each(by, times = nrows[[i]])
      ans[[i]] <- cbind(by_rep, ans[[i]])
    }
  }
  for (i in seq_along(ans))
    rownames(ans[[i]]) <- NULL
  ans
}


## HAS_TESTS
#' Draw from the Posterior Distribution for Parameters
#' from Fitted Model - One Combination of 'By' Variables
#'
#' One combination of 'by' variables can have
#' results from 1 or 'n_draw' fitted models.
#'
#' @param fitted_by List (of length 1 or 'n_draw') of lists
#' each of which has elements 'mean' and 'prec'
#' @param spec Object of class 'BayesProj_spec_ts'
#' @param n_draw Number of draws from posterior
#' @param timevar Name of time variable
#' @param labels_time Vector of labels for historical periods,
#' including periods with no data
#'
#' @returns A names list of data frames.
#'
#' @noRd
draw_post_fit_by <- function(fitted_by, spec, n_draw, timevar, labels_time) {
  n_fit <- length(fitted_by)
  if (n_fit == 1L) {
    mean <- fitted_by[[1L]]$mean
    prec <- fitted_by[[1L]]$prec
    draws <- draw_post_fit_one(spec = spec,
                               mean = mean,
                               prec = prec,
                               n_draw = n_draw)
  }
  else if (n_fit == n_draw) {
    mean <- lapply(fitted_by, function(x) x[["mean"]])
    prec <- lapply(fitted_by, function(x) x[["prec"]])
    draws <- .mapply(draw_post_fit_one,
                     dots = list(mean = mean,
                                 prec = prec),
                     MoreArgs = list(spec = spec,
                                     n_draw = 1L))
    draws <- do.call(cbind, draws)
  }
  else
    stop("Internal error: Unexpected number of draws.")
  draws <- apply(draws, 1L, identity, simplify = FALSE)
  labels <- make_labels_fit(spec = spec, labels_time = labels_time)
  ans <- data.frame(labels = labels)
  ans$.probability <- draws
  what <- make_what_fit(spec = spec, labels_time = labels_time)
  ans <- split(ans, what)
  vname <- make_vname_fit(spec = spec, timevar = timevar)
  for (i in seq_along(ans))
    names(ans[[i]])[[1L]] <- vname[[i]]
  ans
}


## HAS_TESTS
#' Draw from the Posterior Distribution for Parameters
#' for Forecasted Model - All Combinations of 'By' Variables
#'
#' @param x Object of class "BayeProj_proj"
#'
#' @returns A names list of data frames.
#'
#' @noRd
draw_post_proj <- function(x) {
  data <- x$data
  indvar <- x$indvar
  timevar <- x$timevar
  byvar <- x$byvar
  spec_ts <- x$spec_ts
  by <- x$by
  n_draw <- x$n_draw
  labels_time_project <- x$labels_time_project
  benchmarks <- x$benchmarks
  seed_draws_proj <- x$seed_draws_proj
  draws_post_fitted <- draw_post_fit(x)
  par_final <- get_par_final(spec_ts = spec_ts,
                             draws_post = draws_post_fitted,
                             timevar = timevar,
                             byvar = byvar)
  hyper <- get_hyper(draws = draws_post_fitted,
                     byvar = byvar)
  join_on_byvar <- function(x, y) merge(x, y, by = byvar)
  inputs <- list(par_final,
                 hyper,
                 benchmarks)
  inputs <- Reduce(join_on_byvar, inputs)
  par_final <- inputs$.par_final
  hyper <- inputs$.hyper
  benchmarks <- inputs$.benchmarks
  seed_restore <- sample.int(n = .Machine$integer.max, size = 1L) ## create random seed
  set.seed(seed_draws_proj) ## set pre-determined seed
  ans <-  .mapply(draw_post_proj_by,
                  dots = list(par_final = par_final,
                              hyper = hyper,
                              benchmarks = benchmarks),
                  MoreArgs = list(spec_ts = spec_ts,
                                  n_draw = n_draw,
                                  labels_time_project = labels_time_project,
                                  indvar = indvar,
                                  timevar = timevar))
  set.seed(seed_restore) ## restore randomness
  nrows <- vapply(ans[[1L]], nrow, 1L)
  rbind_list_elements <- function(x, y)
    .mapply(rbind, dots = list(x, y), MoreArgs = list())
  ans <- Reduce(rbind_list_elements, ans)
  names(ans) <- names(nrows)
  if (ncol(by) > 0L) {
    for (i in seq_along(ans)) {
      by_rep <- vctrs::vec_rep_each(by, times = nrows[[i]])
      ans[[i]] <- cbind(by_rep, ans[[i]])
    }
  }
  for (i in seq_along(ans))
    rownames(ans[[i]]) <- NULL
  ans
}


## HAS_TESTS
#' Draw from the Posterior Distribution for Parameters
#' from Forecasts -  One Combination of 'By' Variables
#'
#' @param spec_ts Object of class `"BayesProj_spec_ts"`
#' @param par_final Numeric vector with final posterior
#' draws from historical estimates
#' @param hyper Numeric vector with posterior
#' draws for hyper-parameters
#' @param benchmarks Named list with elements
#' 'mean' and 'sd'
#' @param n_draw Number of draws
#' @param labels_time_project Character vector with labels for
#' future periods
#' @param indvar Name of indicator
#' @param timevar Name of the time variable
#'
#' @returns Named list of data frames.
#'
#' @noRd
draw_post_proj_by <- function(spec_ts,
                              par_final,
                              hyper,
                              benchmarks,
                              n_draw,
                              labels_time_project,
                              indvar,
                              timevar) {
  draws <- vector(mode = "list", length = n_draw)
  has_benchmarks <- !is.null(benchmarks)
  if (!has_benchmarks) {
    n_period <- length(labels_time_project)
    mean_bench <- rep(0, times = n_period)
    sd_bench <- rep(Inf, times = n_period)
  }
  for (i_draw in seq_len(n_draw)) {
    if (has_benchmarks) {
      benchmarks_draw <- benchmarks[[i_draw]]
      mean_bench <- benchmarks_draw$mean
      sd_bench <- benchmarks_draw$sd
    }
    hyper_draw <- hyper[[i_draw]]
    par_final_draw <- par_final[[i_draw]]
    prec_proj <- make_prec_proj(spec = spec_ts,
                                hyper = hyper_draw,
                                sd_bench = sd_bench)
    mean_proj <- make_mean_proj(spec = spec_ts,
                                par_final = par_final_draw,
                                hyper = hyper_draw,
                                mean_bench = mean_bench,
                                sd_bench = sd_bench,
                                prec_proj = prec_proj)
    draw <- rmvn(n = 1L,
                 mean = mean_proj,
                 prec = prec_proj)
    draw <- c(draw, hyper_draw)
    draws[[i_draw]] <- draw
  }
  draws <- transpose_list(draws)
  labels <- make_labels_proj(spec = spec_ts,
                             labels_time_project = labels_time_project)
  ans <- data.frame(labels = labels)
  ans$.probability <- draws
  what <- make_what_proj(spec = spec_ts,
                         indvar = indvar,
                         labels_time_project = labels_time_project)
  ans <- split(ans, what)
  vname <- make_vname_proj(spec = spec_ts,
                           timevar = timevar)
  for (i in seq_along(ans))
    names(ans[[i]])[[1L]] <- vname[[i]]
  ans
}


## HAS_TESTS
#' Format Hyper-Parameters from Fitted Model
#'
#' @param Draws returned by function 'draw_post_fit'
#' @param byvar Names of 'by' variables
#'
#' @returns A tibble with the 'by' variables
#' plus a list column called 'hyper'
#'
#' @noRd
get_hyper <- function(draws, byvar) {
  hyper <- draws[["hyper"]]
  hyper <- vctrs::vec_split(x = hyper[[".probability"]],
                            by = hyper[byvar])
  ans <- hyper$key
  val <- hyper$val
  val <- lapply(val, function(x) matrix(unlist(x), ncol = length(x)))
  val <- lapply(val, function(x) apply(x, 1L, identity, simplify = FALSE))
  ans$.hyper <- val
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Make Data Frame Holding Values for Benchmarks
#'
#' @param `x` Object of class `"BayesProj_proj"`
#'
#' @returns A data frame or NULL
#'
#' @noRd
make_benchmarks_df <- function(x) {
  benchmarks <- x$benchmarks
  data <- x$data
  by <- x$by
  indvar <- x$indvar
  timevar <- x$timevar
  labels_time_project <- x$labels_time_project
  val <- benchmarks$.benchmarks
  no_benchmarks <- all(vapply(val, is.null, TRUE))
  if (no_benchmarks)
    return(NULL)
  n_by <- nrow(by)
  n_period <- length(labels_time_project)
  ans <- vctrs::vec_rep_each(by, times = n_period)
  ans[[timevar]] <- rep(labels_time_project, times = n_by)
  is_ind_list <- is.list(data[[indvar]])
  if (is_ind_list) {
    ans$.mean <- unlist(lapply(val, function(x) transpose_list(lapply(x, function(y) y$mean))),
                        recursive = FALSE)
    ans$.sd <- unlist(lapply(val, function(x) x[[1L]]$sd))
  }
  else {
    ans$.mean <- unlist(lapply(val, function(x) x[[1L]]$mean))
    ans$.sd <- unlist(lapply(val, function(x) x[[1L]]$sd))
  }
  ans <- tibble::as_tibble(ans)
  ans
}


## HAS_TESTS
#' Create Credible Intervals from List Column with Draws
#'
#' Modify a data frame so it uses ".probability"
#' columns to create ".fitted", and ".lower", ".upper"
#' columns.
#'
#' Uses matrixStats::colQuantiles for speed.
#'
#' @param x A data frame
#' @param interval With of credible intervals
#'
#' @returns A data frame
#'
#' @noRd
make_credible_intervals <- function(x, interval) {
  probs <- make_probs(interval)
  i_prob <- match(".probability", names(x), nomatch = 0L)
  if (i_prob == 0L)
    stop("internal error: no variable called '.probability'")
  m <- matrix(unlist(x[[i_prob]]),
              nrow = nrow(x),
              byrow = TRUE)
  q <- matrixStats::rowQuantiles(m, probs = probs)
  vals <- data.frame(.fitted = q[, 2L],
                     .lower = q[, 1L],
                     .upper = q[, 3L])
  ans <- append(x, vals, after = i_prob - 1L)
  ans <- tibble::as_tibble(ans)
  ans
}


## HAS_TESTS
#' Given a Credible Interval Width,
#' Make a 'probs' Argument
#'
#' @param interval A number between 0 and 1.
#'
#' @returns A numeric vector of length 3.
#'
#' @noRd
make_probs <- function(interval) {
    checkmate::assert_number(interval, lower = 0, upper = 1)
    alpha <- 1 - interval
    c(0.5 * alpha, 0.5, 1 - 0.5 * alpha)
}

