
## HAS_TESTS
#' Check 'data' Argument for 'model_ts'
#'
#' @param data A data frame
#' @param indvar Name of the indicator variable.
#' @param timevar Name of the time variable.
#' @param byvar Names of additional
#' classification variables.
#' (May have length 0.)
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_data <- function(data,
                       indvar,
                       timevar,
                       byvar) {
    if (indvar == timevar)
        stop("'indvar' and 'timevar' identical")
    if (indvar %in% byvar)
        stop("'indvar' and 'byvar' overlap")
    if (timevar %in% byvar)
        stop("'timevar' and 'byvar' overlap")
    nms_classif_vars <- c(timevar, byvar)
    ## is data frame with at least 1 row, and at least 2 columns
    checkmate::check_data_frame(data,
                                 min.rows = 1L,
                                 min.cols = 2L,
                                 any.missing = FALSE)
    nms_data <- names(data)
    ## has all required variables
    for (vname in c(indvar, nms_classif_vars)) {
        if (!(vname %in% nms_data))
            stop(gettextf("'%s' does not have a variable called \"%s\"",
                          "data",
                          vname),
                 call. = FALSE)
    }
    ## check time var
    timevar_val <- data[[timevar]]
    check_time <- checkmate::check_integerish(timevar_val,
                                              any.missing = FALSE)
    if (!isTRUE(check_time)) {
        stop(gettextf("problem with variable '%s' in '%s' : %s",
                      timevar,
                      "data",
                      check_time),
             call. = FALSE)
    }
    ## check all combinations of classif vars present
    classif_vars_curr <- data[nms_classif_vars]
    levels_classif_vars <- lapply(classif_vars_curr, unique)
    classif_vars_all <- expand.grid(levels_classif_vars,
                                    KEEP.OUT.ATTRS = FALSE,
                                    stringsAsFactors = FALSE)
    paste_dots <- function(...) paste(..., sep = ".")
    id_curr <- do.call(paste_dots, classif_vars_curr)
    id_all <- do.call(paste_dots, classif_vars_all)
    is_in_curr <- id_all %in% id_curr
    i_not_in_curr <- match(FALSE, is_in_curr, nomatch = 0L)
    if (i_not_in_curr > 0L) {
        str_missing <- sprintf("    %s: %s",
                               nms_classif_vars,
                               unlist(classif_vars_all[i_not_in_curr, ]))
        str_missing <- paste(str_missing, collapse = "\n")
        stop(gettextf(paste0("'%s' missing combination of classification variables:\n",
                             "%s"),
                      "data",
                      str_missing),
             call. = FALSE)
    }
    ## check indvar
    indvar_val <- data[[indvar]]
    if (is.numeric(indvar_val))
        check_indvar <- checkmate::check_numeric(indvar_val, finite = TRUE)
    else if (is.list(indvar_val)) {
        check_indvar <- TRUE
        if (length(indvar_val) > 1L) {
            lengths <- lengths(indvar_val)
            if (any(lengths != lengths[[1L]])) {
                check_indvar <- "elements have different lengths"
            }
        }
        if (isTRUE(check_indvar))
            check_indvar <- checkmate::check_numeric(unlist(indvar_val), finite = TRUE)
    }
    else
        check_indvar <- gettextf("has class \"%s\"", class(indvar_val))
    if (!isTRUE(check_indvar))
        stop(gettextf("problem with variable '%s' in '%s' : %s",
                      indvar,
                      "data",
                      check_indvar),
             call. = FALSE)
    ## all ok
    invisible(TRUE)
}


## NO_TESTS
#' Check 'data' Argument for 'Benchmarks'
#'
#' @param data A data frame
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_data_benchmarks <- function(data) {
  ## is data frame with at least 1 row, at least 3 columns,
  ## and no missing values
  checkmate::check_data_frame(data,
                              min.rows = 1L,
                              min.cols = 3L,
                              any.missing = FALSE)
  nms_data <- names(data)
  ## check quantile variables
  for (vname in c("q50", "q90")) {
    if (!(vname %in% nms_data))
      stop(gettextf("'%s' does not have a variable called \"%s\"",
                    "data",
                    vname),
           call. = FALSE)
    val <- data[[vname]]
    if (!is.numeric(val))
      stop(gettextf("'%s' variable has class \"%s\"",
                    vname,
                    class(val)),
           call. = FALSE)
  }
  q50 <- data[["q50"]]
  q90 <- data[["q90"]]
  is_le <- q90 <= q50
  i_le <- match(TRUE, is_le, nomatch = 0L)
  if (i_le > 0L)
    stop(gettextf("element %d of '%s' [%s] is less than or equal to element %d of '%s' [%s]",
                  i_le,
                  "q90",
                  q90[[i_le]],
                  i_le,
                  "q50",
                  q50[[i_le]]),
         call. = FALSE)
  ## check no duplicate rows
  nms_nonq <- setdiff(nms_data, c("q50", "q90"))
  nonq <- data[nms_nonq]
  is_dup <- duplicated(nonq)
  i_dup <- match(TRUE, is_dup, nomatch = 0L)
  if (i_dup > 0L) {
    str_dup <- sprintf("    %s: %s",
                       nms_nonq,
                       unlist(nonq[i_dup, ]))
    str_dup <- paste(str_dup, collapse = "\n")
    stop(gettextf(paste0("'%s' has duplicate values:\n",
                         "%s"),
                  "data",
                  str_dup),
         call. = FALSE)
  }
  ## all OK
  invisible(TRUE)
}


## HAS_TESTS
#' Check that 'fitted' and 'spec_bench' are Consistent with Each Other
#'
#' @param fitted Object of class `"BayesProj_fitted"
#' @param spec_bench Object of class `"BayesProj_spec_bench"`
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_fitted_spec_bench_compatible <- function(fitted, spec_bench) {
  timevar <- fitted$timevar
  byvar <- fitted$byvar
  by <- fitted$by
  log <- fitted$log
  data_bench <- spec_bench$data
  nms_bench <- names(data_bench)
  ## 'spec_bench' has 'timevar'
  if (!(timevar %in% nms_bench))
    stop(gettextf("time variable from '%s' ['%s'] not found in '%s'",
                  "fitted",
                  timevar,
                  "spec_bench"),
         call. = FALSE)
  ## 'by' variables from 'spec_bench' are a subset of those from 'fitted'
  nms_bench_q <- c(timevar, "q50", "q90")
  nms_bench_by <- setdiff(nms_bench, nms_bench_q)
  is_in_byvar <- nms_bench_by %in% byvar
  i_not_in_byvar <- match(FALSE, is_in_byvar, nomatch = 0L)
  if (i_not_in_byvar > 0L)
    stop(gettextf("'%s' has variable ['%s'] not found in '%s'",
                  "spec_bench",
                  nms_bench_by[[i_not_in_byvar]],
                  "fitted"),
         call. = FALSE)
  ## 'spec_bench' has a value for every combination in 'by'
  vars_bench <- data_bench[nms_bench_by]
  vars_fitted <- by[nms_bench_by]
  id_bench <- do.call(paste, vars_bench)
  id_fitted <- do.call(paste, vars_fitted)
  is_in_bench <- id_fitted %in% id_bench
  i_not_in_bench <- match(FALSE, is_in_bench, nomatch = 0L)
  if (i_not_in_bench > 0L) {
    str_not_in_bench <- sprintf("    %s: %s",
                                nms_bench_by,
                                unlist(vars_fitted[i_not_in_bench, ]))
    str_not_in_bench <- paste(str_not_in_bench, collapse = "\n")
    stop(gettextf(paste0("combination of 'by' variables found in '%s' but not in '%s':\n",
                         "%s"),
                  "fitted",
                  "spec_bench",
                  str_not_in_bench),
         call. = FALSE)
  }
  ## quantiles all positive if 'log' is true
  if (log) {
    q50 <- data_bench[["q50"]]
    is_pos <- q50 > 0
    i_nonpos <- match(FALSE, is_pos, nomatch = 0L)
    if (i_nonpos > 0L)
      stop(gettextf("'%s' is TRUE, but '%s' in '%s' has value [%s] less than or equal to 0",
                    "log",
                    "q50",
                    "spec_bench",
                    q50[[i_nonpos]]),
           call. = FALSE)
  }
  ## all tests passed
  invisible(TRUE)
}


## HAS_TESTS
#' Check that a scalar is greater than 0
#'
#' @param x Scalar
#' @param nm Name to be used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_gt_zero <- function(x, nm) {
    if (x <= 0)
        stop(gettextf("'%s' is less than or equal to %d",
                      nm,
                      0L),
             call. = FALSE)
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'fitted' has Class 'BayesProj_fitted'
#'
#' @param fitted
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_fitted <- function(fitted) {
    if (!inherits(fitted, "BayesProj_fitted"))
    stop(gettextf("'%s' has class \"%s\"",
                  "fitted",
                  class(fitted)),
         call. = FALSE)
    invisible(TRUE)
}


## HAS_TESTS
#' Check that 'spec_bench' has Class 'BayesProj_spec_bench'
#'
#' @param spec_bench
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_spec_bench <- function(spec_bench) {
    if (!inherits(spec_bench, "BayesProj_spec_bench"))
    stop(gettextf("'%s' has class \"%s\"",
                  "spec_bench",
                  class(spec_bench)),
         call. = FALSE)
    invisible(TRUE)
}


## HAS_TESTS
#' Check 'log' argument
#'
#' @param log TRUE or FALSE
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_log <- function(log) {
    if (!identical(length(log), 1L))
        stop(gettextf("'%s' does not have length 1",
                      "log"),
             call. = FALSE)
    if (!is.logical(log))
        stop(gettextf("'%s' has class \"%s\"",
                      "log",
                      class(log)),
             call. = FALSE)
    if (is.na(log))
        stop(gettextf("'%s' is NA",
                      "log"),
             call. = FALSE)
    invisible(TRUE)
}


## HAS_TESTS
#' Check that Time Labels Used by 'spec_bench' All
#' Included in 'time_labels'
#'
#' @param time_labels Character vector of time labels
#' @param spec_bench Object of class `"BayesProj_spec_bench"`
#' @param timevar Name of the time variable
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_time_labels_spec_bench_compatible <- function(time_labels,
                                                    spec_bench,
                                                    timevar) {
  data <- spec_bench$data
  time_bench <- data[[timevar]]
  is_in_time_labels <- time_bench %in% time_labels
  i_not_in_time_labels <- match(FALSE, is_in_time_labels, nomatch = 0L)
  if (i_not_in_time_labels > 0L)
    stop(gettextf("'%s' has time label [\"%s\"] not found in '%s'",
                  "spec_bench",
                  time_bench[[i_not_in_time_labels]],
                  "fitted"),
         call = FALSE)
  invisible(TRUE)
}
