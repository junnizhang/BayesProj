
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

test_that("'n_draw' works", {
  set.seed(0)
  data <- expand.grid(time = 2001:2010, sex = c("F", "M"), region = c("A", "B"))
  data$y <- rnorm(n = nrow(data))
  fit <- fit_ts(data = data,
                indvar = "y",
                byvar = c("sex", "region"))
  expect_identical(fit$n_draw, 1000L)
  n_draw(fit) <- 1L
  expect_identical(fit$n_draw, 1L)
})
