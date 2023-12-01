
## 'format_timevar' -----------------------------------------------------------

test_that("'format_timevar' works when time variable is not factor", {
  data <- data.frame(tm = 1:5)
  ans_obtained <- format_timevar(data = data, timevar = "tm")
  ans_expected <- data.frame(tm = factor(1:5))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_timevar' works when time variable is a factor", {
  data <- data.frame(tm = factor(1:5, levels = 5:1))
  ans_obtained <- format_timevar(data = data, timevar = "tm")
  expect_identical(ans_obtained, data)
})


## 'nest_data' ----------------------------------------------------------------

test_that("'nest_data' works when no 'byvar', and 'indvar' is numeric", {
  set.seed(0)
  data <- data.frame(time = 1:5, y = rnorm(5))
  ans_obtained <- nest_data(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = character())
  ans_expected <- data.frame(val = 1)
  ans_expected$val <- list(data[2:1])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'nest_data' works when no 'byvar', and 'indvar' is list", {
  set.seed(0)
  data <- data.frame(time = 1:5)
  data$y <- replicate(n = 5, rnorm(3), simplify = FALSE)
  ans_obtained <- nest_data(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = character())
  ans_expected <- data.frame(val = NA)
  ans_expected$val <- list(data[2:1])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'nest_data' works when has 'byvar', and 'indvar' is numeric", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("F", "M"), each = 5),
                     y = rnorm(10))
  ans_obtained <- nest_data(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = "sex")
  ans_expected <- data.frame(sex = c("F", "M"))
  ans_expected$val <- list(data[1:5, c(3, 1)],
                           data[6:10, c(3, 1)])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'nest_data' works when has 'byvar', and 'indvar' is list", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("F", "M"), each = 5))
  data$y <- replicate(n = 10, rnorm(3), simplify = FALSE)
  ans_obtained <- nest_data(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = "sex")
  ans_expected <- data.frame(sex = c("F", "M"))
  ans_expected$val <- list(data[1:5, c(3, 1)],
                           data[6:10, c(3, 1)])
  expect_identical(ans_obtained, ans_expected)
})


## 'prepare_inputs_fit' -------------------------------------------------------

test_that("prepare_inputs_fit' works with no 'by' variable, numeric indicator", {
  data <- data.frame(time = 1:5, y = 101:105)
  ans_obtained <- prepare_inputs_fit(data = data,
                                     indvar = "y",
                                     timevar = "time",
                                     byvar = character(),
                                     log = FALSE)
  ans_expected <- data.frame(val = NA)
  ans_expected$val <- list(list(as.double(101:105)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("prepare_inputs_fit' works with no 'by' variable, list indicator", {
  data <- data.frame(time = 1:2)
  data$y <- list(c(101, 102), c(103, 104))
  ans_obtained <- prepare_inputs_fit(data = data,
                                     indvar = "y",
                                     timevar = "time",
                                     byvar = character(),
                                     log = TRUE)
  ans_expected <- data.frame(val = NA)
  ans_expected$val <- list(list(log(c(101, 103)),
                                log(c(102, 104))))
  expect_identical(ans_obtained, ans_expected)
})


## 'tab_ind_vs_time' ----------------------------------------------------------

test_that("tab_ind_vs_time' works when indicator variable is numeric", {
  set.seed(0)
  data <- data.frame(val = c(NA, NA))
  data$val <- list(data.frame(y = rnorm(4), time = factor(c(1:2, 4:5), levels = 1:5)),
                   data.frame(y = rnorm(5), time = factor(1:5)))
  ans_obtained <- tab_ind_vs_time(data = data,
                                  indvar = "y",
                                  timevar = "time")
  ans_expected <- data
  ans_expected$val <- list(list(c(data$val[[1]]$y[1:2], NA, 1 * data$val[[1]]$y[3:4])),
                           list(data$val[[2]]$y))
  expect_identical(ans_obtained, ans_expected)
})

test_that("tab_ind_vs_time' works when indicator variable is list", {
  set.seed(0)
  df <- data.frame(time = factor(1:2))
  df$y <- replicate(n = 2, rnorm(3), simplify = FALSE)
  data <- data.frame(val = c(NA, NA))
  data$val <- list(df, df)
  ans_obtained <- tab_ind_vs_time(data = data,
                                  indvar = "y",
                                  timevar = "time")
  v <- list(c(df$y[[1]][1], df$y[[2]][1]),
            c(df$y[[1]][2], df$y[[2]][2]),
            c(df$y[[1]][3], df$y[[2]][3]))
  ans_expected <- data
  ans_expected$val <- list(v, v)
  expect_identical(ans_obtained, ans_expected)
})


## 'tab_ind_vs_time_one' ------------------------------------------------------

test_that("tab_ind_vs_time_one' works when indicator variable is numeric", {
  set.seed(0)
  df <- data.frame(y = rnorm(4), time = factor(c(1:2, 4:5), levels = 1:5))
  ans_obtained <- tab_ind_vs_time_one(df = df,
                                      indvar = "y",
                                      timevar = "time")
  ans_expected <- c(df$y[1:2], NA, 1 * df$y[3:4])
  ans_expected <- list(ans_expected)
  expect_identical(ans_obtained, ans_expected)
})

test_that("tab_ind_vs_time_one' works when indicator variable is list", {
  set.seed(0)
  df <- data.frame(time = factor(1:2))
  df$y <- replicate(n = 2, rnorm(3), simplify = FALSE)
  ans_obtained <- tab_ind_vs_time_one(df = df,
                                      indvar = "y",
                                      timevar = "time")
  ans_expected <- list(c(df$y[[1]][1], df$y[[2]][1]),
                       c(df$y[[1]][2], df$y[[2]][2]),
                       c(df$y[[1]][3], df$y[[2]][3]))
  expect_identical(ans_obtained, ans_expected)
})


## 'take_log_ind' -------------------------------------------------------------

test_that("take_log_ind' works when log is TRUE", {
  set.seed(0)
  df <- data.frame(time = factor(1:2))
  df$y <- replicate(n = 2, abs(rnorm(3)), simplify = FALSE)
  data <- data.frame(val = c(NA, NA))
  data$val <- list(df, df)
  data <- tab_ind_vs_time(data = data,
                          indvar = "y",
                          timevar = "time")
  ans_obtained <- take_log_ind(data, log = TRUE)
  ans_expected <- data
  ans_expected$val[[1]] <- lapply(ans_expected$val[[1]], log)
  ans_expected$val[[2]] <- lapply(ans_expected$val[[2]], log)
  expect_identical(ans_obtained, ans_expected)
})

test_that("take_log_ind' works when log is FALSE", {
  set.seed(0)
  df <- data.frame(time = factor(1:2))
  df$y <- replicate(n = 2, abs(rnorm(3)), simplify = FALSE)
  data <- data.frame(val = c(NA, NA))
  data$val <- list(df, df)
  data <- tab_ind_vs_time(data = data,
                          indvar = "y",
                          timevar = "time")
  expect_identical(take_log_ind(data, log = FALSE),
                   data)
})


## 'transpose_list' -----------------------------------------------------------

test_that("'transpose_list' works with valid inputs", {
  l <- list(1:3, 4:6)
  ans_obtained <- transpose_list(l)
  ans_expected <- list(c(1L, 4L), c(2L, 5L), c(3L, 6L))
  expect_identical(ans_obtained, ans_expected)
})


