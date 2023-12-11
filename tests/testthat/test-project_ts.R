
test_that("'project_ts' creates valid object - no bench, no by", {
  set.seed(0)
  data <- data.frame(time = 1:5)
  data$y <- replicate(5, rnorm(10), simplify = FALSE)
  fitted <- fit_ts(data, indvar = "y")
  ans <- project_ts(fitted = fitted,
                    time_labels = 6:11)
  expect_s3_class(ans, "BayesProj_proj")
  expect_identical(length(ans$mean_bench), 1L)
  expect_true(all(is.infinite(unlist(ans$sd_bench))))
  expect_identical(length(ans$by), 0L)
})

test_that("'project_ts' creates valid object - has bench, no by", {
  set.seed(0)
  data <- data.frame(time = 1:5)
  data$y <- replicate(5, rnorm(10), simplify = FALSE)
  fitted <- fit_ts(data, indvar = "y")
  bench <- Benchmarks(data.frame(time = 10, q50 = 7, q90 = 11))
  ans <- project_ts(fitted = fitted,
                    time_labels = 6:11,
                    spec_bench = bench)
  expect_s3_class(ans, "BayesProj_proj")
  expect_identical(length(ans$mean_bench), 1L)
  expect_identical(length(ans$by), 0L)
})

test_that("'project_ts' creates valid object - no bench, has by", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     reg = rep(c("a", "b"), each = 5))
  data$y <- replicate(10, rnorm(10), simplify = FALSE)
  fitted <- fit_ts(data, indvar = "y", byvar = "reg")
  ans <- project_ts(fitted = fitted,
                    time_labels = 6:11)
  expect_s3_class(ans, "BayesProj_proj")
  expect_identical(length(ans$mean_bench), 2L)
  expect_true(all(is.infinite(unlist(ans$sd_bench))))
  expect_identical(length(ans$by), 1L)
})

test_that("'project_ts' creates valid object - has bench, has by", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     reg = rep(c("a", "b"), each = 5))
  data$y <- replicate(10, rnorm(10), simplify = FALSE)
  fitted <- fit_ts(data, indvar = "y", byvar = "reg")
  bench <- Benchmarks(data.frame(time = 10, q50 = 7, q90 = 11))
  ans <- project_ts(fitted = fitted,
                    time_labels = 6:11,
                    spec_bench = bench)
  expect_s3_class(ans, "BayesProj_proj")
  expect_identical(length(ans$mean_bench), 2L)
  expect_identical(length(ans$by), 1L)
})



