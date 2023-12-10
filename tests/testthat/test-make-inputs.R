
## 'expand_val_bench' ---------------------------------------------------------

test_that("'expand_val_bench' works when data has no gaps", {
  x <- data.frame(tm = factor(2001:2005),
                  q50 = as.numeric(1:5),
                  q90 = as.numeric(2:6))
  ans_obtained <- expand_val_bench(x, timevar = "tm")
  ans_expected <- x
  expect_identical(ans_obtained, ans_expected)
})

test_that("'expand_val_bench' works when data has gaps", {
  x <- data.frame(tm = factor(c(2002, 2004:2005), levels = 2001:2005),
                  q50 = as.numeric(1:3),
                  q90 = as.numeric(2:4))
  ans_obtained <- expand_val_bench(x, timevar = "tm")
  ans_expected <- data.frame(tm = factor(2001:2005),
                             q50 = c(NA, 1, NA, 2, 3),
                             q90 = c(NA, 2, NA, 3, 4))
  expect_identical(ans_obtained, ans_expected)
})



## 'format_timevar' -----------------------------------------------------------

test_that("'format_timevar' works when time variable is not factor - has all levels", {
  data <- data.frame(tm = 1:5)
  ans_obtained <- format_timevar(data = data, timevar = "tm")
  ans_expected <- data.frame(tm = factor(1:5))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_timevar' works when time variable is not factor", {
  data <- data.frame(tm = c(1, 4:5))
  ans_obtained <- format_timevar(data = data, timevar = "tm")
  ans_expected <- data.frame(tm = factor(c(1, 4:5), levels = 1:5))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_timevar' works when time variable is a factor", {
  data <- data.frame(tm = factor(1:5, levels = 5:1))
  ans_obtained <- format_timevar(data = data, timevar = "tm")
  expect_identical(ans_obtained, data)
})


## 'make_i_bench' -------------------------------------------------------------

test_that("make_i_bench' works with valid inputs - no 'by'", {
  by_fitted <- as.data.frame(matrix(nr = 1, nc = 0))
  by_bench <- as.data.frame(matrix(nr = 1, nc = 0))
  ans_obtained <- make_i_bench(by_fitted = by_fitted, by_bench = by_bench)
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)
})

test_that("make_i_bench' works with valid inputs - has 'by'", {
  by_fitted <- expand.grid(a = 1:3, b = 1:2, c = 1:2)
  by_bench <- expand.grid(a = 1:4, c = 1:2)
  ans_obtained <- make_i_bench(by_fitted = by_fitted, by_bench = by_bench)
  ans_expected <- c(1:3, 1:3, 5:7, 5:7)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_mean_bench' ----------------------------------------------------------

test_that("make_mean_bench' works with log = FALSE", {
  x <- data.frame(tm = 2001:2005,
                  q50 = 1:5)
  ans_obtained <- make_mean_bench(x, log = FALSE)
  ans_expected <- 1:5
  expect_identical(ans_obtained, ans_expected)
})

test_that("make_mean_bench' works with log = TRUE", {
  x <- data.frame(tm = 2001:2005,
                  q50 = 1:5)
  ans_obtained <- make_mean_bench(x, log = TRUE)
  ans_expected <- log(1:5)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_mean_sd_bench' -------------------------------------------------------

test_that("make_mean_sd_bench' works - no by", {
  data <- data.frame(time = 1:5, y = 101:105)
  fitted <- fit_ts(data, indvar = "y")
  bench <- Benchmarks(data.frame(time = 8, q50 = 7, q90 = 10))
  ans_obtained <- make_mean_sd_bench(spec_bench = bench,
                                     timevar = "time",
                                     byvar = character(),
                                     by = fitted$by,
                                     time_labels = 6:8,
                                     log = FALSE)
  ans_expected <- list(mean = list(c(NA, NA, 7)),
                       sd = list(c(NA, NA, 3 / qnorm(0.9))))
  expect_identical(ans_obtained, ans_expected)
})

test_that("make_mean_sd_bench' works - has by", {
  data <- data.frame(time = c(1:5, 1:5),
                     sex = rep(c("F", "M"), each = 5),
                     y = c(2:6, 0:4))
  fitted <- fit_ts(data, indvar = "y", byvar = "sex")
  bench <- Benchmarks(data.frame(sex = c("F", "M", "M"),
                                 time = c(8, 7, 8),
                                 q50 = c(7, 4, 3),
                                 q90 = c(8, 6, 7)))
  ans_obtained <- make_mean_sd_bench(spec_bench = bench,
                                     timevar = "time",
                                     byvar = "sex",
                                     by = fitted$by,
                                     time_labels = 6:8,
                                     log = TRUE)
  ans_expected <- list(mean = list(c(NA, NA, log(7)),
                                   c(NA, log(4), log(3))),
                       sd = list(c(NA, NA, (log(8) - log(7)) / qnorm(0.9)),
                                 c(NA, (log(6) - log(4)) / qnorm(0.9),
                                 (log(7) - log(3)) / qnorm(0.9))))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_sd_bench' ------------------------------------------------------------

test_that("make_sd_bench' works with log = FALSE", {
  x <- data.frame(tm = 2001:2005,
                  q50 = 1:5,
                  q90 = 2:6)
  ans_obtained <- make_sd_bench(x, log = FALSE)
  ans_expected <- rep(1 / qnorm(0.9), times = 5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("make_sd_bench' works with log = TRUE", {
  x <- data.frame(tm = 2001:2005,
                  q50 = 1:5,
                  q90 = 2:6)
  ans_obtained <- make_sd_bench(x, log = TRUE)
  ans_expected <- (log(2:6) - log(1:5)) / qnorm(0.9)
  expect_identical(ans_obtained, ans_expected)
})


## 'prepare_inputs_fit' -------------------------------------------------------

test_that("prepare_inputs_fit' works with no 'by' variable, numeric indicator", {
  data <- data.frame(time = 1:5, y = 101:105)
  ans <- prepare_inputs_fit(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = character(),
                            log = FALSE)
  ans_obtained <- ans$val
  ans_expected <- list(list(as.double(101:105)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("prepare_inputs_fit' works with no 'by' variable, list indicator", {
  data <- data.frame(time = 1:2)
  data$y <- list(c(101, 102), c(103, 104))
  ans <- prepare_inputs_fit(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = character(),
                            log = TRUE)
  ans_obtained <- ans$val
  ans_expected <- list(list(log(c(101, 103)),
                            log(c(102, 104))))
  expect_identical(ans_obtained, ans_expected)
})


## 'tab_ind_vs_time' ----------------------------------------------------------

test_that("tab_ind_vs_time' works when indicator variable is numeric", {
  set.seed(0)
  data <- data.frame(val = c(NA, NA))
  data$val <- list(data.frame(y = rnorm(4), time = factor(c(1:2, 4:5), levels = 1:5)),
                   data.frame(y = rnorm(5), time = factor(1:5)))
  ans_obtained <- tab_ind_vs_time(data = data,
                                  indvar = "y",
                                  timevar = "time")
  ans_expected <- data
  ans_expected$val <- list(list(c(data$val[[1]]$y[1:2], NA, 1 * data$val[[1]]$y[3:4])),
                           list(data$val[[2]]$y))
  expect_identical(ans_obtained, ans_expected)
})

test_that("tab_ind_vs_time' works when indicator variable is list", {
  set.seed(0)
  df <- data.frame(time = factor(1:2))
  df$y <- replicate(n = 2, rnorm(3), simplify = FALSE)
  data <- data.frame(val = c(NA, NA))
  data$val <- list(df, df)
  ans_obtained <- tab_ind_vs_time(data = data,
                                  indvar = "y",
                                  timevar = "time")
  v <- list(c(df$y[[1]][1], df$y[[2]][1]),
            c(df$y[[1]][2], df$y[[2]][2]),
            c(df$y[[1]][3], df$y[[2]][3]))
  ans_expected <- data
  ans_expected$val <- list(v, v)
  expect_identical(ans_obtained, ans_expected)
})


## 'tab_ind_vs_time_one' ------------------------------------------------------

test_that("tab_ind_vs_time_one' works when indicator variable is numeric", {
  set.seed(0)
  df <- data.frame(y = rnorm(4), time = factor(c(1:2, 4:5), levels = 1:5))
  ans_obtained <- tab_ind_vs_time_one(df = df,
                                      indvar = "y",
                                      timevar = "time")
  ans_expected <- c(df$y[1:2], NA, 1 * df$y[3:4])
  ans_expected <- list(ans_expected)
  expect_identical(ans_obtained, ans_expected)
})

test_that("tab_ind_vs_time_one' works when indicator variable is list", {
  set.seed(0)
  df <- data.frame(time = factor(1:2))
  df$y <- replicate(n = 2, rnorm(3), simplify = FALSE)
  ans_obtained <- tab_ind_vs_time_one(df = df,
                                      indvar = "y",
                                      timevar = "time")
  ans_expected <- list(c(df$y[[1]][1], df$y[[2]][1]),
                       c(df$y[[1]][2], df$y[[2]][2]),
                       c(df$y[[1]][3], df$y[[2]][3]))
  expect_identical(ans_obtained, ans_expected)
})
