


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
