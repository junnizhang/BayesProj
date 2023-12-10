
## HAS_TESTS
#' Expand Quantile Data Frame to Include Missing Years
#'
#' Time variable is a factor.
#' 
#' @param x A data frame with variables `q50`, `q90`,
#' and <timevar>.
#' @param Name of time variable
#'
#' @returns A data frame
#'
#' @noRd
expand_val_bench <- function(x, timevar) {
  q50 <- tapply(x[["q50"]], x[[timevar]], identity)
  q90 <- tapply(x[["q90"]], x[[timevar]], identity)
  ans <- data.frame(time = factor(levels(x[[timevar]])),
                    q50 = unname(q50),
                    q90 = unname(q90))
  names(ans)[[1L]] <- timevar
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
#' Get the Row Number for Elements of 'spec_bench' that Align
#' with Elements of 'fitted'
#'
#' @param by_fitted 'by' variables for 'fitted'
#' @param by_bench 'by' variables for 'spec_bench'
#'
#' @returns An integer vector
#'
#' @noRd
make_i_bench <- function(by_fitted, by_bench) {
  has_by_fitted <- length(by_fitted)
  has_by_bench <- length(by_bench)
  if (has_by_fitted && has_by_bench) {
    nms_bench <- names(by_bench)
    id_fitted <- do.call(paste, by_fitted[nms_bench])
    id_bench <- do.call(paste, by_bench)
    match(id_fitted, id_bench, nomatch = 0L)
  }
  else
    rep(1L, times = nrow(by_fitted))
}


## HAS_TESTS
#' Turn Benchmark Quantiles into Means
#'
#' @param x A data frame with variable `q50`
#' @param log Whether to apply log
#'
#' @returns A data frame
#'
#' @noRd
make_mean_bench <- function(x, log) {
  q50 <- x[["q50"]]
  if (log) log(q50) else q50
}


#' Use Information in 'spec_bench' to Create Lists of Benchmark
#' Means and Standard Deviations for Each Combination of 'by'
#' Variables in 'fitted'
#'
#' @param spec_bench Object of class `"BayesProj_spec_bench"`
#' @param timevar Name of the time variable used in `fitted`
#' @param byvar Names of the by variables used in `fitted`.
#' Can have length 0.
#' @param by Data frame with by variables from `fitted`
#' @param time_labels Character vector of time labels
#' for periods in projection interval
#' @param log Whether to model on the log scale
#'
#' @returns A named list of numeric vectors
#'
#' @noRd
make_mean_sd_bench <- function(spec_bench,
                               timevar,
                               byvar,
                               by,
                               time_labels,
                               log) {
  data <- spec_bench$data
  nms <- names(data)
  nms_q <- c(timevar, "q50", "q90")
  nms_by <- setdiff(nms, nms_q)
  data[[timevar]] <- factor(data[[timevar]], levels = time_labels)
  data <- vctrs::vec_split(x = data[nms_q], by = data[nms_by])
  val_bench <- data$val
  by_bench <- data$key
  val_bench <- lapply(val_bench, expand_val_bench, timevar = timevar)
  mean <- lapply(val_bench, make_mean_bench, log = log)
  sd <- lapply(val_bench, make_sd_bench, log = log)
  i_bench <- make_i_bench(by_fitted = by,
                          by_bench = by_bench)
  mean <- mean[i_bench]
  sd <- sd[i_bench]
  list(mean = mean,
       sd = sd)
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
  data <- vctrs::vec_split(data[c(indvar, timevar)],
                           data[byvar])
  data <- tab_ind_vs_time(data = data,
                          indvar = indvar,
                          timevar = timevar)
  data <- take_log_ind(data = data,
                       log = log)
  data
}


## HAS_TESTS
#' Turn Benchmark Quantiles into Standard Deviations
#'
#' @param x A data frame with variable `q50`
#' @param log Whether to apply log
#'
#' @returns A data frame
#'
#' @noRd
make_sd_bench <- function(x, log) {
  z90 <- qnorm(0.9)
  q50 <- x[["q50"]]
  q90 <- x[["q90"]]
  if (log) {
    q50 <- log(q50)
    q90 <- log(q90)
  }
  (q90 - q50) / z90
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
