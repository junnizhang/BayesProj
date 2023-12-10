

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
  ans <- lapply(fitted,
                draw_post_fit_by,
                spec = spec,
                n_draw = n_draw,
                timevar = timevar,
                labels_time = labels_time)
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
  ans
}


## NO_TESTS
#' Draw from the Posterior Distribution for Parameters
#' for Forecasted Model - All Combinations of 'By' Variables
#'
#' @param x Object of class "BayeProj_proj"
#'
#' @returns A names list of data frames.
#'
#' @noRd
draw_post_proj <- function(x) {
  timevar <- x$timevar
  spec_ts <- x$spec_ts
  by <- x$by
  n_draw <- x$n_draw
  mean_bench <- x$mean_bench
  sd_bench <- x$sd_bench
  draws_post_fitted <- draws_post_fit(x)
  par_final <- get_par_final(spec = spec_ts,
                             draws_post = draws_post_fitted)
  hyper <- draws_post_fitted[["hyper"]]
  ans <- .mapply(draw_post_proj_by,
                 dots = list(par_final = par_final,
                             hyper = hyper[[".probability"]],
                             mean_bench = mean_bench,
                             sd_bench = sd_bench),
                 MoreArgs = list(spec_ts = spec_ts))
  ## PROCESS SOME MORE
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


## NO_TESTS
#' Draw from the Posterior Distribution for Parameters
#' from Forecasts -  One Combination of 'By' Variables
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
draw_post_proj_by <- function(spec_ts,
                              par_final,
                              hyper,
                              mean_bench,
                              sd_bench,
                              n_draw) {
  ans <- vector(mode = "list", length = n_draw)
  for (i_draw in seq_len(n_draw)) {
    mean_proj <- make_mean_proj(spec = spec_ts,
                                par_final = par_final[[i_draw]],
                                hyper = hyper[[i_draw]],
                                mean_bench = mean_bench,
                                sd_bench = sd_bench)
    prec_proj <- make_prec_proj(spec = spec_ts,
                                hyper = hyper[[i_draw]],
                                sd_bench = sd_bench)
    draw <- rmvn(n = 1L,
                 mean = mean_proj,
                 prec = prec_proj)
    draw <- split_draw_proj(spec = spec_ts,
                            draw = draw)
    ans[[i_draw]] <- draw
  }
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

