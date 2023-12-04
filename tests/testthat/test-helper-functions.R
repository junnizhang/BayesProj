

## 'draw_post_fit' ---------------------------------------------------------

test_that("draw_post_fit' works with valid inputs - indvar is numeric, no by", {
  set.seed(0)
  data <- data.frame(time = 1:5, y = rnorm(5, mean = 1:20))
  fit <- fit_ts(data,
                indvar = "y",
                spec_ts = DampedTrend())
  ans <- draw_post_fit(fit)
  expect_setequal(names(ans), c("level", "trend", "hyper"))
  expect_true(is.data.frame(ans$level))
})

test_that("draw_post_fit' works with valid inputs - indvar is list, no by", {
  set.seed(0)
  data <- data.frame(time = 1:5)
  data$y <- replicate(n = 5, rnorm(20), simplify = FALSE)
  fit <- fit_ts(data,
                indvar = "y",
                spec_ts = DampedTrend())
  ans <- draw_post_fit(fit)
  expect_setequal(names(ans), c("level", "trend", "hyper"))
  expect_true(is.data.frame(ans$level))
})

test_that("draw_post_fit' works with valid inputs - indvar is numeric, has by", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, times = 2),
                     sex = rep(c("f", "m"), each = 5))
  data$y <- rnorm(10)
  fit <- fit_ts(data,
                indvar = "y",
                byvar = "sex",
                spec_ts = DampedTrend())
  ans <- draw_post_fit(fit)
  expect_setequal(names(ans), c("level", "trend", "hyper"))
  expect_true(is.data.frame(ans$level))
  expect_identical(names(ans$level)[[1L]], "sex")
})

test_that("draw_post_fit' works with valid inputs - indvar is list, has by", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, times = 2),
                     sex = rep(c("f", "m"), each = 5))
  data$y <- replicate(n = 10, rnorm(5), simplify = FALSE)
  fit <- fit_ts(data,
                indvar = "y",
                byvar = "sex",
                spec_ts = DampedTrend())
  ans <- draw_post_fit(fit)
  expect_setequal(names(ans), c("level", "trend", "hyper"))
  expect_true(is.data.frame(ans$level))
  expect_identical(names(ans$level)[[1L]], "sex")
})


## 'draw_post_fit_by' ---------------------------------------------------------

test_that("draw_post_fit_by' works with valid inputs - indvar is numeric ", {
  set.seed(0)
  data <- data.frame(time = 1:5, y = rnorm(5, mean = 1:20))
  inputs <- prepare_inputs_fit(data = data,
                               indvar = "y",
                               timevar = "time",
                               byvar = character(),
                               log = FALSE)
  spec <- DampedTrend()
  fitted <- make_fitted(inputs = inputs, spec = spec)
  ans <- draw_post_fit_by(fitted_by = fitted[[1]],
                          spec = spec,
                          n_draw = 10L,
                          timevar = "time",
                          labels_time = 1:5)
  expect_setequal(names(ans), c("level", "trend", "hyper"))
  expect_identical(names(ans$level)[[1L]], "time")
  expect_identical(names(ans$trend)[[1L]], "time")
  expect_identical(names(ans$hyper)[[1L]], "hyper")
})

test_that("draw_post_fit_by' works with valid inputs - indvar is list ", {
  set.seed(0)
  data <- data.frame(time = 1:5)
  data$y <- replicate(n = 5, rnorm(10), simplify = FALSE)
  inputs <- prepare_inputs_fit(data = data,
                               indvar = "y",
                               timevar = "time",
                               byvar = character(),
                               log = FALSE)
  spec <- DampedTrend()
  fitted <- make_fitted(inputs = inputs, spec = spec)
  ans <- draw_post_fit_by(fitted_by = fitted[[1]],
                          spec = spec,
                          n_draw = 10L,
                          timevar = "time",
                          labels_time = 1:5)
  expect_setequal(names(ans), c("level", "trend", "hyper"))
  expect_identical(names(ans$level)[[1L]], "time")
  expect_identical(names(ans$trend)[[1L]], "time")
  expect_identical(names(ans$hyper)[[1L]], "hyper")
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


## 'invlogit' -----------------------------------------------------------------

test_that("'invlogit' works with valid inputs", {
  x <- c(0, -Inf, Inf, NA, 3, -3)
  ans_obtained <- invlogit(x)
  ans_expected <- c(0.5, 0, 1, NA, 1 / (1 + exp(-3)), exp(-3) / (1 + exp(-3)))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_credible_intervals' --------------------------------------------------

test_that("'make_credible_intervals' works with valid inputs", {
  set.seed(0)
  x <- data.frame(time = 1:2)
  x$.probability <- list(rnorm(10), rnorm(10))
  ans_obtained <- make_credible_intervals(x, interval = 0.9)
  ans_expected <- data.frame(time = 1:2,
                             .fitted = sapply(x$.probability, median, USE.NAMES = FALSE),
                             .lower = sapply(x$.probability, quantile, 0.05, USE.NAMES = FALSE),
                             .upper = sapply(x$.probability, quantile, 0.95, USE.NAMES = FALSE))
  ans_expected$.probability <- x$.probability
  ans_expected <- tibble::tibble(ans_expected)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_credible_intervals' throws expected error with no '.probability' column", {
  x <- data.frame(time = 1:2)
  expect_error(make_credible_intervals(x, interval = 0.9),
               "internal error: no variable called '.probability'")
})  



## 'make_probs' ---------------------------------------------------------------

test_that("'make_probs' works", {
    expect_equal(make_probs(0.95),
                 c(0.025, 0.5, 0.975))
    expect_equal(make_probs(0),
                 c(0.5, 0.5, 0.5))
    expect_equal(make_probs(1L),
                 c(0, 0.5, 1))
    expect_error(make_probs(1.1))
    expect_error(make_probs(-1))
})


## 'nest_data' ----------------------------------------------------------------

test_that("'nest_data' works when no 'byvar', and 'indvar' is numeric", {
  set.seed(0)
  data <- data.frame(time = 1:5, y = rnorm(5))
  ans_obtained <- nest_data(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = character())
  ans_expected <- data.frame(val = 1)
  ans_expected$val <- list(data[2:1])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'nest_data' works when no 'byvar', and 'indvar' is list", {
  set.seed(0)
  data <- data.frame(time = 1:5)
  data$y <- replicate(n = 5, rnorm(3), simplify = FALSE)
  ans_obtained <- nest_data(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = character())
  ans_expected <- data.frame(val = NA)
  ans_expected$val <- list(data[2:1])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'nest_data' works when has 'byvar', and 'indvar' is numeric", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("F", "M"), each = 5),
                     y = rnorm(10))
  ans_obtained <- nest_data(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = "sex")
  ans_expected <- data.frame(sex = c("F", "M"))
  ans_expected$val <- list(data[1:5, c(3, 1)],
                           data[6:10, c(3, 1)])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'nest_data' works when has 'byvar', and 'indvar' is list", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("F", "M"), each = 5))
  data$y <- replicate(n = 10, rnorm(3), simplify = FALSE)
  ans_obtained <- nest_data(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = "sex")
  ans_expected <- data.frame(sex = c("F", "M"))
  ans_expected$val <- list(data[1:5, c(3, 1)],
                           data[6:10, c(3, 1)])
  expect_identical(ans_obtained, ans_expected)
})


## 'prepare_inputs_fit' -------------------------------------------------------

test_that("prepare_inputs_fit' works with no 'by' variable, numeric indicator", {
  data <- data.frame(time = 1:5, y = 101:105)
  ans_obtained <- prepare_inputs_fit(data = data,
                                     indvar = "y",
                                     timevar = "time",
                                     byvar = character(),
                                     log = FALSE)
  ans_expected <- data.frame(val = NA)
  ans_expected$val <- list(list(as.double(101:105)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("prepare_inputs_fit' works with no 'by' variable, list indicator", {
  data <- data.frame(time = 1:2)
  data$y <- list(c(101, 102), c(103, 104))
  ans_obtained <- prepare_inputs_fit(data = data,
                                     indvar = "y",
                                     timevar = "time",
                                     byvar = character(),
                                     log = TRUE)
  ans_expected <- data.frame(val = NA)
  ans_expected$val <- list(list(log(c(101, 103)),
                                log(c(102, 104))))
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


