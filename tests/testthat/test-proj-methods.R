## 'augment' ------------------------------------------------------------------

test_that("augment' works without benchmarks", {
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
  ans <- augment(projected)
  expect_setequal(names(ans), c("time", "sex",
                                ".fitted", ".lower", ".upper", ".probability"))
  expect_identical(nrow(ans), 10L)
})

test_that("augment' works with benchmarks", {
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
  ans <- augment(projected)
  expect_setequal(names(ans), c("time", "sex", ".bench.lower", ".bench.mid", ".bench.upper",
                                ".fitted", ".lower", ".upper", ".probability"))
  expect_identical(nrow(ans), 10L)
})

test_that("augment' gives same answer when called twice", {
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
  ans0 <- augment(projected)
  ans1 <- augment(projected)
  expect_identical(ans0, ans1)
})



## 'components' ---------------------------------------------------------------

test_that("components' works with benchmarks, has by, indvar num", {
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
  ans <- components(projected, what = "benchmarks")
  expect_true(is.data.frame(ans))
  expect_identical(names(ans), c("sex", "time", ".mean", ".sd"))
  expect_identical(nrow(ans), 10L)
})

test_that("components' works with benchmarks, has by, indvar list", {
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
  ans <- components(projected)
  expect_identical(names(ans), c("y", "level", "trend", "hyper", "benchmarks"))
  expect_true(all(sapply(ans, tibble::is_tibble)))
})

test_that("components' works with benchmarks, no by, indvar list", {
  set.seed(0)
  data <- data.frame(time = 1:5)
  data$y <- replicate(n = 5, rnorm(5), simplify = FALSE)
  fitted <- fit_ts(data,
                   indvar = "y",
                   spec_ts = DampedTrend())
  spec_bench <- Benchmarks(data.frame(time = 10,
                                      q50 = 11,
                                      q90 = 11.1))
  projected <- project_ts(fitted,
                          time_labels = 6:10,
                          spec_bench = spec_bench)
  ans <- components(projected)
  expect_identical(nrow(ans$y), 5L)
})

test_that("components' gives same answer when called twice", {
  set.seed(0)
  data <- data.frame(time = 1:5)
  data$y <- replicate(n = 5, rnorm(5), simplify = FALSE)
  fitted <- fit_ts(data,
                   indvar = "y",
                   spec_ts = DampedTrend())
  spec_bench <- Benchmarks(data.frame(time = 10,
                                      q50 = 11,
                                      q90 = 11.1))
  projected <- project_ts(fitted,
                          time_labels = 6:10,
                          spec_bench = spec_bench)
  ans0 <- components(projected)
  ans1 <- components(projected)
  expect_identical(ans0, ans1)
})


## 'n_draw' -------------------------------------------------------------------

test_that("n_draw' works - indicator is numeric", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("f", "m"), each = 5),
                     y = rnorm(10, mean = 11:15))
  fitted <- fit_ts(data,
                   indvar = "y",
                   byvar = "sex",
                   spec_ts = DampedTrend())
  expect_identical(n_draw(fitted), 1000L)
  n_draw(fitted) <- 5
  expect_identical(n_draw(fitted), 5L)
  spec_bench <- Benchmarks(data.frame(time = 10,
                                      q50 = 11,
                                      q90 = 11.1))
  projected <- project_ts(fitted,
                          time_labels = 6:10,
                          spec_bench = spec_bench)
  expect_identical(n_draw(projected), 5L)
  n_draw(projected) <- 10L
  expect_identical(n_draw(projected), 10L)
})


test_that("n_draw' works - indicator is list", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("f", "m"), each = 5))
  data$y <- replicate(n = 10, list(rnorm(20, mean = 11:13)))
  fitted <- fit_ts(data,
                   indvar = "y",
                   byvar = "sex",
                   spec_ts = DampedTrend())
  expect_identical(n_draw(fitted), 20L)
  n_draw(fitted) <- 10
  expect_identical(n_draw(fitted), 10L)
  spec_bench <- Benchmarks(data.frame(time = 10,
                                      q50 = 11,
                                      q90 = 11.1))
  projected <- project_ts(fitted,
                          time_labels = 6:10,
                          spec_bench = spec_bench)
  expect_identical(n_draw(projected), 10L)
  n_draw(projected) <- 5L
  expect_identical(n_draw(projected), 5L)
  expect_error(n_draw(projected) <- 20,
               "replacement value for 'n_draw' \\[20\\] greater than number of draws in inputs \\[5\\]")
})

