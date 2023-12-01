
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
