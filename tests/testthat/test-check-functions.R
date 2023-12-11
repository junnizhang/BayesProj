
## 'check_data' ---------------------------------------------------------------

test_that("'check_data' returns TRUE with valid value - list value, no by", {
  data <- data.frame(time = 1:3)
  data$value <- list(1:2, 2:3, 3:4)
  expect_true(check_data(data = data,
                         indvar = "value",
                         timevar = "time",
                         byvar = character()))
})

test_that("'check_data' returns TRUE with valid value - numeric value, has by", {
  data <- data.frame(time = rep(1:3, times = 2),
                     sex = rep(c("F", "M"), each = 3),
                     value = 1:6)
  expect_true(check_data(data = data,
                         indvar = "value",
                         timevar = "time",
                         byvar = "sex"))
})

test_that("'check_data' throws correct error when indvar and timevar same", {
  data <- data.frame(time = 1:3)
  expect_error(check_data(data = data,
                          indvar = "time",
                          timevar = "time",
                          byvar = character()),
               "'indvar' and 'timevar' identical")
})

test_that("'check_data' throws correct error when indvar and timevar same", {
  data <- data.frame(time = 1:3,
                     var = 3:1)
  expect_error(check_data(data = data,
                          indvar = "var",
                          timevar = "time",
                          byvar = "var"),
               "'indvar' and 'byvar' overlap")
})

test_that("'check_data' throws correct error when indvar and timevar same", {
  data <- data.frame(time = 1:3,
                     var = 3:1)
  expect_error(check_data(data = data,
                          indvar = "var",
                          timevar = "time",
                          byvar = "time"),
               "'timevar' and 'byvar' overlap")
})

test_that("'check_data' throws correct error when does not have indvar", {
  data <- data.frame(time = 1:3,
                     var = 3:1)
  expect_error(check_data(data = data,
                          indvar = "wrong",
                          timevar = "time",
                          byvar = character()),
               "'data' does not have a variable called \"wrong\"")
})

test_that("'check_data' throws correct error when timevar faulty", {
  data <- data.frame(time = c("2000-2005", "2005-2010", "2010-2015"),
                     var = 3:1)
  expect_error(check_data(data = data,
                          indvar = "var",
                          timevar = "time",
                          byvar = character()),
               "problem with variable 'time' in 'data' :")
})

test_that("'check_data' throws correct error when missing combination of variables", {
  data <- expand.grid(time = 1:3,
                      age = 1:2,
                      sex = c("f", "m"))
  data$var <- 1:12
  data <- data[-12,]
  expect_error(check_data(data = data,
                          indvar = "var",
                          timevar = "time",
                          byvar = c("age", "sex")),
               "'data' missing combination of classification variables:")
})

test_that("'check_data' throws correct error when list indvar faulty", {
  set.seed(0)
  data <- expand.grid(time = 1:3,
                      age = 1:2,
                      sex = c("f", "m"))
  data$var <- replicate(n = 12, rnorm(2), simplify = FALSE)
  data$var[[5]] <- rnorm(3)
  expect_error(check_data(data = data,
                          indvar = "var",
                          timevar = "time",
                          byvar = c("age", "sex")),
               "problem with variable 'var' in 'data' : elements have different lengths")
})

test_that("'check_data' throws correct error when indvar wrong class", {
  set.seed(0)
  data <- expand.grid(time = 1:3,
                      age = 1:2,
                      sex = c("f", "m"))
  data$var <- letters[1:12]
  expect_error(check_data(data = data,
                          indvar = "var",
                          timevar = "time",
                          byvar = c("age", "sex")),
               "problem with variable 'var' in 'data' : has class \"character\"")
})


## 'check_fitted_spec_bench_compatible' ---------------------------------------

test_that("'check_fitted_spec_becnh_compatible' returns TRUE with valid inputs - no 'by'", {
  data <- data.frame(time = 2020,
                     val = 3)
  fitted <- fit_ts(data, indvar = "val")
  spec_bench <- Benchmarks(data.frame(time = 2025, q50 = 2, q90 = 3))
  expect_true(check_fitted_spec_bench_compatible(fitted = fitted,
                                                 spec_bench = spec_bench))
})

test_that("'check_fitted_spec_bench_compatible' returns TRUE with valid inputs - has 'by'", {
  data <- data.frame(time = rep(1:3, times = 2),
                     sex = rep(c("F", "M"), each = 3),
                     value = 1:6)
  fitted <- fit_ts(data, indvar = "value", byvar = "sex")
  spec_bench <- Benchmarks(data.frame(time = c(10, 10), q50 = 2:1, q90 = 3:2, sex = c("F", "M")))
  expect_true(check_fitted_spec_bench_compatible(fitted = fitted,
                                                 spec_bench = spec_bench))
})

test_that("'check_fitted_spec_bench_compatible' throws correct error when 'spec_bench' does not have timevar", {
  data <- data.frame(time = rep(1:3, times = 2),
                     sex = rep(c("F", "M"), each = 3),
                     value = 1:6)
  fitted <- fit_ts(data, indvar = "value", byvar = "sex")
  spec_bench <- Benchmarks(data.frame(wrong = c(10, 10), q50 = 2:1, q90 = 3:2, sex = c("F", "M")))
  expect_error(check_fitted_spec_bench_compatible(fitted = fitted,
                                                  spec_bench = spec_bench),
               "time variable from 'fitted' \\['time'\\] not found in 'spec_bench'")
})

test_that("'check_fitted_spec_bench_compatible' throws correct error when 'spec_bench' has by variables not in fitted", {
  data <- data.frame(time = rep(1:3, times = 2),
                     sex = rep(c("F", "M"), each = 3),
                     value = 1:6)
  fitted <- fit_ts(data, indvar = "value", byvar = "sex")
  spec_bench <- Benchmarks(data.frame(time = c(10, 10), q50 = 2:1, q90 = 3:2, wrong = c("F", "M")))
  expect_error(check_fitted_spec_bench_compatible(fitted = fitted,
                                                  spec_bench = spec_bench),
               "'spec_bench' has variable \\['wrong'\\] not found in 'fitted'")
})

test_that("'check_fitted_spec_bench_compatible' throws correct error when 'fitted' has combination of 'by' variables not found in 'spec_bench'", {
  data <- data.frame(time = rep(1:3, times = 3),
                     sex = rep(c("F", "M", "D"), each = 3),
                     value = 1:9)
  fitted <- fit_ts(data, indvar = "value", byvar = "sex")
  spec_bench <- Benchmarks(data.frame(time = c(10, 10), q50 = 2:1, q90 = 3:2, sex = c("F", "M")))
  expect_error(check_fitted_spec_bench_compatible(fitted = fitted,
                                                  spec_bench = spec_bench),
               "combination of 'by' variables found in 'fitted' but not in 'spec_bench':")
})

test_that("'check_fitted_spec_bench_compatible' throws correct error when 'log' is TRUE but q50 non-positive", {
  data <- data.frame(time = rep(1:3, times = 2),
                     sex = rep(c("F", "M"), each = 3),
                     value = 1:6)
  fitted <- fit_ts(data, indvar = "value", byvar = "sex", log = TRUE)
  spec_bench <- Benchmarks(data.frame(time = c(10, 10), q50 = 0:1, q90 = 3:2, sex = c("F", "M")))
  expect_error(check_fitted_spec_bench_compatible(fitted = fitted,
                                                  spec_bench = spec_bench),
               "'log' is TRUE, but 'q50' in 'spec_bench' has value \\[0\\] less than or equal to 0")
})  


## 'check_is_fitted' ----------------------------------------------------------

test_that("'check_is_fitted' returns TRUE with valid input", {
  data <- data.frame(time = 2020,
                     val = 3)
  fitted <- fit_ts(data, indvar = "val")
  expect_true(check_is_fitted(fitted))
})

test_that("'check_is_fitted' throws correct error with invalid input", {
  expect_error(check_is_fitted(NULL),
               "'fitted' has class \"NULL\"")
})


## 'check_gt_zero' ------------------------------------------------------------

test_that("'check_gt_zero' returns TRUE with valid value", {
    expect_true(check_gt_zero(0.01, nm = "x"))
    expect_true(check_gt_zero(Inf, nm = "x"))
})

test_that("'check_gt_zero' returns TRUE with valid value", {
    expect_error(check_gt_zero(0, nm = "x"),
                 "'x' is less than or equal to 0")
    expect_error(check_gt_zero(-3, nm = "y"),
                 "'y' is less than or equal to 0")
})


## 'check_log' ----------------------------------------------------------------

test_that("'check_log' returns TRUE with valid value", {
  expect_true(check_log(TRUE))
  expect_true(check_log(FALSE))
})

test_that("'check_log' throws correct error when 'log' does not have length 1", {
  expect_error(check_log(c(TRUE, FALSE)),
               "'log' does not have length 1")
  expect_error(check_log(logical()),
               "'log' does not have length 1")
})

test_that("'check_log' throws correct error when 'log' not logical", {
  expect_error(check_log("F"),
               "'log' has class \"character\"")
})

test_that("'check_log' throws correct error when 'log' is NA", {
  expect_error(check_log(NA),
               "'log' is NA")
})


## 'check_is_spec_bench' ------------------------------------------------------

test_that("'check_is_spec_bench' returns TRUE with valid input", {
  bench <- Benchmarks(data.frame(time = 2020,
                                 q50 = 3,
                                 q90 = 5))
  expect_true(check_is_spec_bench(bench))
})

test_that("'check_is_spec_bench' throws correct error with invalid input", {
  expect_error(check_is_spec_bench(NULL),
               "'spec_bench' has class \"NULL\"")
})


## 'check_time_labels_spec_bench_compatible' ----------------------------------

test_that("'check_time_labels_spec_bench_compatible' returns TRUE with valid input", {
  time_labels <- c("2015-2020", "2020-2025", "2025-2030", "2030-2035")
  bench <- Benchmarks(data.frame(time = c("2020-2025", "2025-2030"),
                                 q50 = 3:4,
                                 q90 = 5:6))
  timevar <- "time"
  expect_true(check_time_labels_spec_bench_compatible(time_labels = time_labels,
                                                      spec_bench = bench,
                                                      timevar = timevar))
})

test_that("'check_time_labels_spec_bench_compatible' returns correct error with mismatched labels", {
  time_labels <- c("2015-2020", "2020-2025", "2025-2030", "2030-2035")
  bench <- Benchmarks(data.frame(time = c("2020-2025", "wrong"),
                                 q50 = 3:4,
                                 q90 = 5:6))
  timevar <- "time"
  expect_error(check_time_labels_spec_bench_compatible(time_labels = time_labels,
                                                      spec_bench = bench,
                                                      timevar = timevar),
               "'spec_bench' has time label \\[\"wrong\"\\] not found in 'fitted'")
})

  
  
