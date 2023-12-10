
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
