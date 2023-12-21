
test_that("'fit_ts' works when indicator is numeric", {
  data <- expand.grid(time = 2001:2010, sex = c("F", "M"), region = c("A", "B"))
  data$y <- rnorm(n = nrow(data))
  ans <- fit_ts(data = data,
                indvar = "y",
                byvar = c("sex", "region"))
  expect_s3_class(ans, "BayesProj_fitted")
  expect_identical(ans$labels_time, as.character(2001:2010))
  expect_false(ans$log)
  expect_identical(ans$n_draw, 1000L)
})

test_that("'fit_ts' works when indicator is list", {
  data <- expand.grid(time = 2001:2010, sex = c("F", "M"), region = c("A", "B"))
  data$y <- replicate(n = nrow(data), rnorm(10), simplify = FALSE)
  ans <- fit_ts(data = data,
                indvar = "y",
                byvar = c("sex", "region"))
  expect_s3_class(ans, "BayesProj_fitted")
  expect_identical(ans$labels_time, as.character(2001:2010))
  expect_false(ans$log)
  expect_identical(ans$n_draw, 10L)
})

test_that("'fit_ts' throws correct error when indvar starts with .", {
  data <- expand.grid(time = 2001:2010, sex = c("F", "M"), region = c("A", "B"))
  data$.y <- rnorm(n = nrow(data))
  expect_error(fit_ts(data = data,
                      indvar = ".y",
                      byvar = c("sex", "region")),
               "'indvar' starts with a '.'")
})

test_that("'fit_ts' throws correct error when timevar starts with .", {
  data <- expand.grid(.time = 2001:2010, sex = c("F", "M"), region = c("A", "B"))
  data$y <- rnorm(n = nrow(data))
  expect_error(fit_ts(data = data,
                      indvar = "y",
                      timevar = ".time",
                      byvar = c("sex", "region")),
               "'timevar' starts with a '.'")
})

test_that("'fit_ts' throws correct error when byvar starts with .", {
  data <- expand.grid(time = 2001:2010, sex = c("F", "M"), .region = c("A", "B"))
  data$y <- rnorm(n = nrow(data))
  expect_error(fit_ts(data = data,
                      indvar = "y",
                      timevar = "time",
                      byvar = c("sex", ".region")),
               "'byvar' includes a name that starts with a '.'")
})



