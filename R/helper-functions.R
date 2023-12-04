
## HAS_TESTS
#' Draw from the Posterior Distribution for Parameters
#' - for All Combinations of 'By' Variables
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


## HAS_TESTS
#' Draw from the Posterior Distribution for Parameters
#' - for One Combination of 'By' Variables
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
#' Convert Non-Factor Time Variable to Factor
#'
#' Fills in intermediate values.
#'
#' @param Data A data frame
#' @param timevar Name of the time variable
#'
#' @returns A data frame
#'
#' @noRd
format_timevar <- function(data, timevar) {
  timevar_val <- data[[timevar]]
  if (!is.factor(timevar_val)) {
    levels <- seq.int(from = min(timevar_val, na.rm = TRUE),
                      to = max(timevar_val, na.rm = TRUE))
    timevar_val <- factor(timevar_val, levels = levels)
    data[[timevar]] <- timevar_val
  }
  data
}


## HAS_TESTS
#' Inverse Logit Function
#'
#' Guards against overflow, and allows NA.
#' 
#' @param x Numeric vector
#'
#' @returns Numeric vector
#'
#' @noRd
invlogit <- function(x) {
  ans <- rep(NA_real_, times = length(x))
  is_na <- is.na(x)
  is_nonneg <- !is_na & (x >= 0)
  is_neg <- !is_na & (x < 0)
  ans[is_nonneg] <- 1 / (1 + exp(-x[is_nonneg]))
  ans[is_neg] <- exp(x[is_neg]) / (1 + exp(x[is_neg]))
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


## HAS_TESTS
#' Nest Indicator and Time Variables
#' within By Variables
#'
#' Reformat `data` so that, for every
#' combination of the 'by' variables, there
#' is one data frame holding the indicator
#' and time variables. If there are no
#' 'by' variables, then the return value
#' is a data frame with 1 row and 1 column.
#'
#' @param data Data frame
#' @param indvar Name of the indicator variable
#' @param timevar Name of the time variable
#' @param byvar Name(s) of the by variables.
#' Can have length 0.
#'
#' @returns A data frame
#'
#' @noRd
nest_data <- function(data, indvar, timevar, byvar) {
  val <- data[c(indvar, timevar)]
  has_by <- length(byvar) > 0L
  if (has_by) {
    by <- data[byvar]
    val <- split(x = val, f = by)
    ans <- split(x = by, f = by)
    ans <- lapply(ans, unique)
    ans <- do.call(rbind, ans)
    ans$val <- unname(val)
    rownames(ans) <- NULL
  }
  else {
    ans <- data.frame(val = NA)
    ans$val <- list(val)
  }
  ans
}


## HAS_TESTS
#' Prepare Inputs for Fitting Time Series Model
#'
#' @param data Data frame with input data
#' @param indvar Name of indicator variable
#' @param timevar Name of time variable
#' @param byvar Names of by variables. Can have
#' length 0.
#' @param log Whether to transform indicator
#' to log scale.
#'
#' @returns A data frame
#'
#' @noRd
prepare_inputs_fit <- function(data, indvar, timevar, byvar, log) {
  data <- nest_data(data = data,
                    indvar = indvar,
                    timevar = timevar,
                    byvar = byvar)
  data <- tab_ind_vs_time(data = data,
                          indvar = indvar,
                          timevar = timevar)
  data <- take_log_ind(data = data,
                       log = log)
  data
}


## HAS_TESTS
#' Draw from a multivariate normal distribution
#'
#' Code based partly on MASS::mvrnorm.
#' 
#' @param n Number of draws
#' @param mean Mean of MVN distribution
#' @param prec Precision of MVN distribution
#'
#' @returns Matrix with length(mean)
#' rows and n columns.
#'
#' @noRd
rmvn <- function(n, mean, prec) {
    n_val <- length(mean)
    ch <- chol(prec)
    I <- diag(n_val)
    sd <- backsolve(ch, I)
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    t(mean + sd %*% Z)
}


## HAS_TESTS
#' Tabulate the Indicator Variable
#'
#' Apply 'tab_ind_vs_time_one' to each data frame
#' in the 'val' column of 'data'
#'
#' @param data Data frame
#' @param indvar Name of the indicator variable
#' @param timevar Name of the time variable
#'
#' @returns A list of vectors
#'
#' @noRd
tab_ind_vs_time <- function(data, indvar, timevar) {
  val <- data[["val"]]
  val <- lapply(X = val,
                FUN = tab_ind_vs_time_one,
                indvar = indvar,
                timevar = timevar)
  data$val <- val
  data
}


## HAS_TESTS
#' Tabulate the Indicator Variable in a Single Dataframe
#'
#' Create a list of tabulations of the indicator
#' variable, with one observation for each time
#' period, with NAs in periods with no data.
#'
#' `df` contains values for a single combination
#' of the 'by' variables. The values in the
#' time variable should be unique.
#'
#' Works when indicator variable is numeric,
#' and when it is a list column.
#'
#' @param df Data frame
#' @param indvar Name of the indicator variable
#' @param timevar Name of the time variable
#'
#' @returns A list of vectors
#'
#' @noRd
tab_ind_vs_time_one <- function(df, indvar, timevar) {
  indvar_val <- df[[indvar]]
  timevar_val <- df[[timevar]]
  ind_is_list <- is.list(indvar_val)
  if (anyDuplicated(timevar_val))
    stop("internal error: time variable has duplicates")
  f <- function(ind, time) as.numeric(tapply(ind, time, identity))
  if (ind_is_list) {
    indvar_val <- transpose_list(indvar_val)
    ans <- lapply(indvar_val, f, time = timevar_val)
  }
  else {
    ans <- f(ind = indvar_val, time = timevar_val)
    ans <- list(ans)
  }
  ans
}


## HAS_TESTS
#' Transform Indicator to Log Scale
#'
#' If `log` is `TRUE`, transform the indicator
#' variable to the log scale.
#'
#' @param data Data frame
#' @param log Whether to take logs
#'
#' @returns A modified version of `data`.
#'
#' @noRd
take_log_ind <- function(data, log) {
  if (log) {
    val <- data[["val"]]
    val <- rapply(val, f = base::log, how = "replace")
    data[["val"]] <- val
  }
  data
}


## HAS_TESTS    
#' Tranpose List
#'
#' Turn a list of length m, each element of which
#' has n entries, to a list of length n,
#' each element of which has m entries.
#'
#' @param l A list
#'
#' @returns A list
#'
#' @noRd
transpose_list <- function(l) {
  m <- matrix(unlist(l), ncol = length(l))
  apply(m, 1L, identity, simplify = FALSE)
}
