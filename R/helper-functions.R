
## HAS_TESTS
#' Convert Non-Factor Time Variable to Factor
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
    timevar_val <- factor(timevar_val)
    data[[timevar]] <- timevar_val
  }
  data
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
  data <- format_timevar(data = data,
                         timevar = timevar)
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
