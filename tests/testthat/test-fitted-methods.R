
## 'augment' method for 'BayesProj_fitted' ---------------------------------

test_that("'augment' works when 'log' is FALSE", {
  data <- expand.grid(time = 2001:2010, sex = c("F", "M"), region = c("A", "B"))
  data$y <- rnorm(n = nrow(data))
  fit <- fit_ts(data = data,
                indvar = "y",
                byvar = c("sex", "region"))
  ans <- augment(fit)
  expect_identical(names(ans), c("time", "sex", "region", "y",
                                 ".fitted", ".lower", ".upper",
                                 ".probability"))
})

test_that("'augment' works when 'log' is TRUE", {
  data <- expand.grid(time = 2001:2010, sex = c("F", "M"), region = c("A", "B"))
  data$y <- runif(n = nrow(data))
  fit <- fit_ts(data = data,
                indvar = "y",
                byvar = c("sex", "region"),
                log = TRUE)
  ans <- augment(fit)
  expect_identical(names(ans), c("time", "sex", "region", "y",
                                 ".fitted", ".lower", ".upper",
                                 ".probability"))
  expect_true(all(unlist(ans$.probability) > 0))
})


## 'components' method for 'BayesProj_fitted' ---------------------------------

test_that("'components' works when 'what' is NULL", {
  data <- expand.grid(time = 2001:2010, sex = c("F", "M"), region = c("A", "B"))
  data$y <- rnorm(n = nrow(data))
  fit <- fit_ts(data = data,
                indvar = "y",
                byvar = c("sex", "region"))
  ans <- components(fit)
  expect_identical(names(ans), c("level", "trend", "hyper"))
})

test_that("'components' works when 'what' is 'level'", {
  data <- expand.grid(time = 2001:2010, sex = c("F", "M"), region = c("A", "B"))
  data$y <- rnorm(n = nrow(data))
  fit <- fit_ts(data = data,
                indvar = "y",
                byvar = c("sex", "region"))
  ans <- components(fit, what = "level")
  expect_identical(names(ans), c("sex", "region", "time",
                                 ".fitted", ".lower", ".upper", ".probability"))
})

test_that("'components' throws correct error with invalid 'what'", {
  data <- expand.grid(time = 2001:2010, sex = c("F", "M"), region = c("A", "B"))
  data$y <- rnorm(n = nrow(data))
  fit <- fit_ts(data = data,
                indvar = "y",
                byvar = c("sex", "region"))
  expect_error(components(fit, what = c("level", "wrong")),
               "Invalid value for 'what' : valid choices are 'level', 'trend', 'hyper'")
})


## 'n_draw' -------------------------------------------------------------------

test_that("'n_draw' works - non-list inputs", {
  set.seed(0)
  data <- expand.grid(time = 2001:2010, sex = c("F", "M"), region = c("A", "B"))
  data$y <- rnorm(n = nrow(data))
  fit <- fit_ts(data = data,
                indvar = "y",
                byvar = c("sex", "region"))
  expect_identical(n_draw(fit), 1000L)
  n_draw(fit) <- 1L
  expect_identical(n_draw(fit), 1L)
})

test_that("'n_draw' works - list inputs", {
  set.seed(0)
  data <- expand.grid(time = 2001:2005, sex = c("F", "M"))
  data$y <- replicate(n = nrow(data), list(rnorm(10)))
  fit <- fit_ts(data = data,
                indvar = "y",
                byvar = "sex")
  expect_identical(n_draw(fit), 10L)
  n_draw(fit) <- 5L
  expect_identical(n_draw(fit), 5L)
  expect_identical(length(fit$data[["y"]][[1L]]), 5L)
  expect_error(n_draw(fit) <- 10,
               "replacement value for 'n_draw' \\[10\\] greater than number of draws in inputs \\[5\\]")
})

