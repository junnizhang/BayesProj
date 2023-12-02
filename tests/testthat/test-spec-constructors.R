
## 'DampedTrend' --------------------------------------------------------------

test_that("'DampedTrend' works with valid inputs - all default", {
  ans <- DampedTrend()
  expect_s3_class(ans, "BayesProj_spec_ts_dampedtrend")
  expect_s3_class(ans, "BayesProj_spec_ts")
})

test_that("'DampedTrend' works with valid inputs - all non-default", {
  ans <- DampedTrend(scale_obs = 0.2,
                     scale_level = 0.25,
                     scale_trend = 0.3,
                     damp_min = 0.5,
                     damp_max = 0.7)
  expect_s3_class(ans, "BayesProj_spec_ts_dampedtrend")
  expect_s3_class(ans, "BayesProj_spec_ts")
  expect_identical(ans$scale_sd_y, 0.2)
})

test_that("'DampedTrend' throws correct error when damp_min equals damp_max", {
  expect_error(DampedTrend(damp_min = 0.5,
                           damp_max = 0.5),
               "'damp_min' is greater than or equal to 'damp_max'")
})


## 'new_BayesProj_spec_ts_dampedtrend' ----------------------------------------

test_that("'new_BayesProj_spec_ts_dampedtrend' works with valid inputs", {
  ans <- new_BayesProj_spec_ts_dampedtrend(scale_sd_y = 0.3,
                                           scale_sd_level = 0.2,
                                           scale_sd_trend = 0.1,
                                           damp_min = 0.7,
                                           damp_max = 0.99)
  expect_s3_class(ans, "BayesProj_spec_ts_dampedtrend")
  expect_s3_class(ans, "BayesProj_spec_ts")
})
                                            
