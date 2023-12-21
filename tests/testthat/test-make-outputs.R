
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


## 'draw_post_proj' ----------------------------------------------------------------

test_that("draw_post_proj' works with damped trend, no by, no bench", {
  set.seed(0)
  data <- data.frame(time = 1:5, y = rnorm(5, mean = 1:5))
  fitted <- fit_ts(data,
                   indvar = "y",
                   spec_ts = DampedTrend())
  n_draw(fitted) <- 5
  projected <- project_ts(fitted, time_labels = 6:10)
  ans <- draw_post_proj(projected)
  expect_identical(names(ans), c("y", "level", "trend", "hyper"))
  expect_true(all(sapply(ans, is.data.frame)))
  expect_true(all(sapply(ans[1:3], nrow) == 5))
})

test_that("draw_post_proj' works with damped trend, no by, has bench", {
  set.seed(0)
  data <- data.frame(time = 1:5, y = rnorm(5, mean = 1:5))
  fitted <- fit_ts(data,
                   indvar = "y",
                   spec_ts = DampedTrend())
  n_draw(fitted) <- 5
  spec_bench <- Benchmarks(data.frame(time = 10, q50 = 10, q90 = 12))
  projected <- project_ts(fitted, time_labels = 6:10, spec_bench = spec_bench)
  ans <- draw_post_proj(projected)
  expect_identical(names(ans), c("y", "level", "trend", "hyper"))
  expect_true(all(sapply(ans, is.data.frame)))
  expect_true(all(sapply(ans[1:3], nrow) == 5))
})

test_that("draw_post_proj' works with damped trend, has by, bench", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("f", "m"), each = 5),
                     y = rnorm(10, mean = 1:10))
  fitted <- fit_ts(data,
                   indvar = "y",
                   byvar = "sex",
                   spec_ts = DampedTrend())
  n_draw(fitted) <- 5
  spec_bench <- Benchmarks(data.frame(time = 10, q50 = 10, q90 = 12))
  projected <- project_ts(fitted, time_labels = 6:10, spec_bench = spec_bench)
  ans <- draw_post_proj(projected)
  expect_identical(names(ans), c("y", "level", "trend", "hyper"))
  expect_true(all(sapply(ans, is.data.frame)))
  expect_true(all(sapply(ans[1:3], nrow) == 2 * 5))
})


## 'draw_post_proj_by' --------------------------------------------------------

test_that("draw_post_proj' works with damped trend", {
  spec_ts <- DampedTrend()
  par_final <- list(1:2, 2:3)
  hyper <- list(1:4, 2:5)
  benchmarks <- list(list(mean = 1:4, sd = c(0.2, 0.3, 0.4, 0.5)),
                     list(mean = 0:3, sd = c(0.25, 0.35, 0.45, 0.55)))
  n_draw <- 2L
  labels_time_project <- 2001:2004
  indvar <- "y"
  timevar <- "time"
  ans <- draw_post_proj_by(spec_ts = spec_ts,
                           par_final = par_final,
                           hyper = hyper,
                           benchmarks = benchmarks,
                           n_draw = n_draw,
                           labels_time_project = labels_time_project,
                           indvar = indvar,
                           timevar = timevar)
  expect_identical(names(ans), c("y", "level", "trend", "hyper"))
  expect_identical(nrow(ans$level), length(benchmarks[[1]]$mean))
  expect_identical(length(ans$level$.probability[[1]]), length(par_final))
})


## 'get_hyper' ----------------------------------------------------------------

test_that("get_hyper' works with damped trend, no by", {
  set.seed(0)
  data <- data.frame(time = 1:5, y = rnorm(5, mean = 1:5))
  fit <- fit_ts(data,
                indvar = "y",
                spec_ts = DampedTrend())
  n_draw(fit) <- 5
  draws <- draw_post_fit(fit)
  ans_obtained <- get_hyper(draws = draws, byvar = fit$byvar)
  ans_expected <- data.frame(.hyper = NA)
  hyper <- matrix(unlist(draws$hyper$.probability), nr = 5)
  hyper <- lapply(1:5, function(i) hyper[i,])
  ans_expected$.hyper <- list(hyper)
  expect_identical(ans_obtained, ans_expected)
})

test_that("get_hyper' works with damped trend, has by", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("f", "m"), each = 5),
                     y = rnorm(10, mean = 1:10))
  fit <- fit_ts(data,
                indvar = "y",
                byvar = "sex",
                spec_ts = DampedTrend())
  n_draw(fit) <- 5
  draws <- draw_post_fit(fit)
  ans_obtained <- get_hyper(draws = draws, byvar = fit$byvar)
  ans_expected <- data.frame(sex = c("f", "m"))
  hyper1 <- matrix(unlist(draws$hyper$.probability[1:4]), nr = 5)
  hyper2 <- matrix(unlist(draws$hyper$.probability[5:8]), nr = 5)
  hyper1 <- lapply(1:5, function(i) hyper1[i,])
  hyper2 <- lapply(1:5, function(i) hyper2[i,])
  ans_expected$.hyper <- list(hyper1, hyper2)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_benchmarks_df' -------------------------------------------------------

test_that("make_benchmarks_df' works with no benchmarks", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("f", "m"), each = 5),
                     y = rnorm(10, mean = 11:15))
  fitted <- fit_ts(data,
                   indvar = "y",
                   byvar = "sex",
                   spec_ts = DampedTrend())
  n_draw(fitted) <- 5
  projected <- project_ts(fitted,
                          time_labels = 6:10)
  ans <- make_benchmarks_df(projected)
  expect_identical(ans, NULL)
})

test_that("make_benchmarks_df' works with benchmarks, has by, indvar num", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("f", "m"), each = 5),
                     y = rnorm(10, mean = 11:15))
  fitted <- fit_ts(data,
                   indvar = "y",
                   byvar = "sex",
                   spec_ts = DampedTrend())
  n_draw(fitted) <- 5
  spec_bench <- Benchmarks(data.frame(sex = c("f", "m"),
                                      time = c(10, 10),
                                      q50 = c(20, 21),
                                      q90 = c(23, 24)))
  projected <- project_ts(fitted,
                          time_labels = 6:10,
                          spec_bench = spec_bench)
  ans <- make_benchmarks_df(projected)
  expect_true(is.data.frame(ans))
  expect_identical(names(ans), c("sex", "time", ".mean", ".sd"))
  expect_identical(nrow(ans), 10L)
})

test_that("make_benchmarks_df' works with benchmarks, has by, indvar list", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("f", "m"), each = 5))
  data$y <- replicate(n = 10, rnorm(5), simplify = FALSE)
  fitted <- fit_ts(data,
                   indvar = "y",
                   byvar = "sex",
                   spec_ts = DampedTrend())
  spec_bench <- Benchmarks(data.frame(sex = c("f", "m"),
                                      time = c(10, 10),
                                      q50 = c(11, 12),
                                      q90 = c(13, 14)))
  projected <- project_ts(fitted,
                          time_labels = 6:10,
                          spec_bench = spec_bench)
  ans <- make_benchmarks_df(projected)
  expect_true(is.data.frame(ans))
  expect_identical(names(ans), c("sex", "time", ".mean", ".sd"))
  expect_identical(nrow(ans), 10L)
})

test_that("make_benchmarks_df' works with benchmarks, no by, indvar list", {
  set.seed(0)
  data <- data.frame(time = 1:5)
  data$y <- replicate(n = 5, rnorm(5), simplify = FALSE)
  fitted <- fit_ts(data,
                   indvar = "y",
                   spec_ts = DampedTrend())
  spec_bench <- Benchmarks(data.frame(time = 10,
                                      q50 = 11,
                                      q90 = 13))
  projected <- project_ts(fitted,
                          time_labels = 6:10,
                          spec_bench = spec_bench)
  ans <- make_benchmarks_df(projected)
  expect_true(is.data.frame(ans))
  expect_identical(names(ans), c("time", ".mean", ".sd"))
  expect_identical(nrow(ans), 5L)
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
