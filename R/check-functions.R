
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
    if (!identical(is.logical(log), 1L))
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
