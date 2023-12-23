## 'composite' ------------------------------------------------------------------

test_that("composite' works without benchmarks, numeric indvar", {
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
  ans <- composite(projected)
  expect_setequal(names(ans), c(".variant", "time", "sex",
                                "y", ".lower", ".upper"))
  expect_identical(nrow(ans), 20L)
})

test_that("composite' works with benchmarks, numeric indvar", {
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
  ans <- composite(projected)
  expect_setequal(names(ans), c(".variant", "time", "sex",
                                ".bench.lower", ".bench.mid", ".bench.upper",
                                "y", ".lower", ".upper"))
  expect_identical(nrow(ans), 20L)
})

test_that("composite' works with benchmarks, list indvar", {
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
                                      q50 = c(20, 21),
                                      q90 = c(23, 24)))
  projected <- project_ts(fitted,
                          time_labels = 6:10,
                          spec_bench = spec_bench)
  ans <- composite(projected)
  expect_setequal(names(ans), c(".variant", "time", "sex",
                                ".bench.lower", ".bench.mid", ".bench.upper",
                                "y", ".lower", ".upper"))
  expect_identical(nrow(ans), 20L)
})



