
test_that("make_fitted' works with no 'by' variable, numeric indicator", {
  set.seed(0)
  data <- data.frame(time = 1:5, y = rnorm(5, mean = 1:20))
  inputs <- prepare_inputs_fit(data = data,
                               indvar = "y",
                               timevar = "time",
                               byvar = character(),
                               log = FALSE)
  spec_ts <- DampedTrend()
  ans <- make_fitted(inputs = inputs, spec_ts = spec_ts)
  expect_true(!anyNA(unlist(ans)))
})

test_that("make_fitted' works with no 'by' variable, list indicator", {
  set.seed(0)
  data <- data.frame(time = 1:5)
  data$y <- replicate(5, rnorm(10), simplify = FALSE)
  inputs <- prepare_inputs_fit(data = data,
                               indvar = "y",
                               timevar = "time",
                               byvar = character(),
                               log = FALSE)
  spec_ts <- DampedTrend()
  ans <- make_fitted(inputs = inputs, spec_ts = spec_ts)
  expect_true(!anyNA(unlist(ans)))
})

test_that("make_fitted' works with with 'by' variable, list indicator, log = TRUE", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, times = 2),
                     reg = rep(c("a", "b"), each = 5))
  data$y <- replicate(10, runif(10), simplify = FALSE)
  inputs <- prepare_inputs_fit(data = data,
                               indvar = "y",
                               timevar = "time",
                               byvar = "reg",
                               log = TRUE)
  spec_ts <- DampedTrend()
  ans <- make_fitted(inputs = inputs, spec_ts = spec_ts)
  expect_true(!anyNA(unlist(ans)))
})


