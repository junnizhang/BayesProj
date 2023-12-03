
## 'draw_post_fit_one' --------------------------------------------------------

test_that("draw_post_fit_one works with DampedTrend and n_draw = 1", {
  set.seed(0)
  spec <- DampedTrend()
  mean <- list(level = rnorm(3),
               trend = rnorm(3),
               hyper = runif(4))
  prec <- matrix(runif(100), nr = 10)
  prec <- crossprod(prec)
  ans <- draw_post_fit_one(spec = spec,
                           mean = mean,
                           prec = prec,
                           n_draw = 1L)
  expect_identical(dim(ans), c(10L, 1L))
  expect_true(all(ans[7:10] > 0))
})

test_that("draw_post_fit_one works with DampedTrend and n_draw = 1000", {
  set.seed(0)
  spec <- DampedTrend()
  mean <- list(level = rnorm(3),
               trend = rnorm(3),
               hyper = runif(4))
  prec <- matrix(runif(100), nr = 10)
  prec <- crossprod(prec)
  ans <- draw_post_fit_one(spec = spec,
                           mean = mean,
                           prec = prec,
                           n_draw = 1000L)
  expect_identical(dim(ans), c(10L, 1000L))
  expect_true(all(ans[7:10,] > 0))
  expect_true(all(ans[10,] < 1))
})

  
## 'make_class_spec' ----------------------------------------------------------

test_that("'make_class_spec' works", {
  expect_identical(make_class_spec(DampedTrend()), "dampedtrend")
})


## 'make_consts' ---------------------------------------------------------------

test_that("'make_consts' works with damped trend", {
  spec <- DampedTrend()
  y <- 1:2
  ans <- make_consts(spec = spec, y = y)
  expect_identical(names(ans),
                   c("sd_level_init",
                     "sd_trend_init",
                     "scale_sd_y",
                     "scale_sd_level",
                     "scale_sd_trend",
                     "damp_min",
                     "damp_max"))
  expect_true(all(ans > 0))
})


## 'make_hyper' ---------------------------------------------------------------

test_that("'make_hyper' works with damped trend", {
  spec <- DampedTrend()
  ans_obtained <- make_hyper(spec = spec)
  ans_expected <- c(log_sd_y = 0,
                    log_sd_level = 0,
                    log_sd_trend = 0,
                    logit_damp = 0)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_labels_fit' ----------------------------------------------------------

test_that("'make_labels_fit' works with damped trend", {
  spec <- DampedTrend()
  labels_time <- 2000:2002
  ans_obtained <- make_labels_fit(spec = spec, labels_time = labels_time)
  ans_expected <- c(2000:2002, 2000:2002, "sd_obs", "sd_level", "sd_trend", "damp")
  expect_identical(ans_obtained, ans_expected)
})


## 'make_level' ---------------------------------------------------------------

test_that("'make_level' works with damped trend", {
  spec <- DampedTrend()
  y <- 1:2
  ans_obtained <- make_level(spec = spec, y = y)
  ans_expected <- c(time1 = 0,
                    time2 = 0)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_parameters' ----------------------------------------------------------

test_that("'make_parameters' works with damped trend", {
  spec <- DampedTrend()
  y <- 1:2
  ans_obtained <- make_parameters(spec = spec, y = y)
  ans_expected <- list(level = c(time1 = 0, time2 = 0),
                       trend = c(time1 = 0, time2 = 0),
                       hyper = c(log_sd_y = 0,
                                 log_sd_level = 0,
                                 log_sd_trend = 0,
                                 logit_damp = 0))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_random' --------------------------------------------------------------

test_that("'make_random' works with damped trend", {
  spec <- DampedTrend()
  ans_obtained <- make_random(spec)
  ans_expected <- c("level", "trend")
  expect_identical(ans_obtained, ans_expected)
})


## 'make_trend' ---------------------------------------------------------------

test_that("'make_trend' works with damped trend", {
  spec <- DampedTrend()
  y <- 1:2
  ans_obtained <- make_trend(spec = spec, y = y)
  ans_expected <- c(time1 = 0,
                    time2 = 0)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_vname_fit' -----------------------------------------------------------

test_that("'make_vname' works with damped trend", {
  spec <- DampedTrend()
  timevar <- "time"
  ans_obtained <- make_vname_fit(spec = spec, timevar = timevar)
  ans_expected <- c("time", "time", "hyper")
  expect_identical(ans_obtained, ans_expected)
})


## 'make_what_fit' ------------------------------------------------------------

test_that("'make_what_fit' works with damped trend", {
  spec <- DampedTrend()
  labels_time <- 2000:2002
  ans_obtained <- make_what_fit(spec = spec, labels_time = labels_time)
  ans_expected <- factor(rep(c("level", "trend", "hyper"), times = c(3, 3, 4)),
                         levels = c("level", "trend", "hyper"))
  expect_identical(ans_obtained, ans_expected)
})
