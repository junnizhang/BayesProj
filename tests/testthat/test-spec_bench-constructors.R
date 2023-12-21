
## 'new_BayesProj_spec_bench' -------------------------------------------------

test_that("'new_BayesProj_spec_bench' works with valid inputs", {
  data <- data.frame(time = c(2030, 2030),
                     sex = c("f", "m"),
                     q50 = c(3, 4),
                     q90 = c(4.1, 5.3))
  ans <- Benchmarks(data)
  expect_s3_class(ans, "BayesProj_spec_bench")
  expect_identical(ans$method, "natural")
  ans <- Benchmarks(data, method = "hyman")
  expect_s3_class(ans, "BayesProj_spec_bench")
  expect_identical(ans$method, "hyman")
})
  
