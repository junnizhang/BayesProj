
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
  y <- 1:2
  ans_obtained <- make_hyper(spec = spec, y = y)
  ans_expected <- c(time1 = 0,
                    time2 = 0,
                    log_sd_y = 0,
                    log_sd_level = 0,
                    log_sd_trend = 0,
                    logit_damp = 0)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_par' -----------------------------------------------------------------

test_that("'make_par' works with damped trend", {
  spec <- DampedTrend()
  y <- 1:2
  ans_obtained <- make_par(spec = spec, y = y)
  ans_expected <- c(time1 = 0,
                    time2 = 0)
  expect_identical(ans_obtained, ans_expected)
})
