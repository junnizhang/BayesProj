
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
  ans <- components(projected, what = "benchmarks")
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
  ans <- components(projected)
  expect_identical(names(ans), c("y", "level", "trend", "hyper", "benchmarks"))
  expect_true(all(sapply(ans, tibble::is_tibble)))
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
                                      q90 = 11.1))
  projected <- project_ts(fitted,
                          time_labels = 6:10,
                          spec_bench = spec_bench)
  ans <- components(projected)
  expect_identical(nrow(ans$y), 5L)
})
