
## 'invlogit' -----------------------------------------------------------------

test_that("'invlogit' works with valid inputs", {
  x <- c(0, -Inf, Inf, NA, 3, -3)
  ans_obtained <- invlogit(x)
  ans_expected <- c(0.5, 0, 1, NA, 1 / (1 + exp(-3)), exp(-3) / (1 + exp(-3)))
  expect_identical(ans_obtained, ans_expected)
})



## 'rmvn' ---------------------------------------------------------------------

test_that("'rmvn' gives correct answer with valid inputs", {
    set.seed(0)
    mean <- rnorm(3)
    prec <- matrix(runif(n = 9, max = 10), nr = 3)
    prec <- t(prec) %*% prec
    ans <- rmvn(n = 100000,
                mean = mean,
                prec = prec)
    expect_equal(colMeans(ans), mean, tolerance = 0.01)
    expect_equal(solve(cov(ans)), prec, tolerance = 0.01)
})


## 'take_log_ind' -------------------------------------------------------------

test_that("take_log_ind' works when log is TRUE", {
  set.seed(0)
  df <- data.frame(time = factor(1:2))
  df$y <- replicate(n = 2, abs(rnorm(3)), simplify = FALSE)
  data <- data.frame(val = c(NA, NA))
  data$val <- list(df, df)
  data <- tab_ind_vs_time(data = data,
                          indvar = "y",
                          timevar = "time")
  ans_obtained <- take_log_ind(data, log = TRUE)
  ans_expected <- data
  ans_expected$val[[1]] <- lapply(ans_expected$val[[1]], log)
  ans_expected$val[[2]] <- lapply(ans_expected$val[[2]], log)
  expect_identical(ans_obtained, ans_expected)
})

test_that("take_log_ind' works when log is FALSE", {
  set.seed(0)
  df <- data.frame(time = factor(1:2))
  df$y <- replicate(n = 2, abs(rnorm(3)), simplify = FALSE)
  data <- data.frame(val = c(NA, NA))
  data$val <- list(df, df)
  data <- tab_ind_vs_time(data = data,
                          indvar = "y",
                          timevar = "time")
  expect_identical(take_log_ind(data, log = FALSE),
                   data)
})


## 'transpose_list' -----------------------------------------------------------

test_that("'transpose_list' works with valid inputs", {
  l <- list(1:3, 4:6)
  ans_obtained <- transpose_list(l)
  ans_expected <- list(c(1L, 4L), c(2L, 5L), c(3L, 6L))
  expect_identical(ans_obtained, ans_expected)
})


