
## 'expand_val_bench' ---------------------------------------------------------

test_that("'expand_val_bench' works when data has no gaps", {
  x <- data.frame(tm = factor(2001:2005),
                  q50 = as.numeric(1:5),
                  q90 = as.numeric(2:6))
  ans_obtained <- expand_val_bench(x, timevar = "tm")
  ans_expected <- x
  expect_identical(ans_obtained, ans_expected)
})

test_that("'expand_val_bench' works when data has gaps", {
  x <- data.frame(tm = factor(c(2002, 2004:2005), levels = 2001:2005),
                  q50 = as.numeric(1:3),
                  q90 = as.numeric(2:4))
  ans_obtained <- expand_val_bench(x, timevar = "tm")
  ans_expected <- data.frame(tm = factor(2001:2005),
                             q50 = c(NA, 1, NA, 2, 3),
                             q90 = c(NA, 2, NA, 3, 4))
  expect_identical(ans_obtained, ans_expected)
})


## 'format_timevar' -----------------------------------------------------------

test_that("'format_timevar' works when time variable is not factor - has all levels", {
  data <- data.frame(tm = 1:5)
  ans_obtained <- format_timevar(data = data, timevar = "tm")
  ans_expected <- data.frame(tm = factor(1:5))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_timevar' works when time variable is not factor", {
  data <- data.frame(tm = c(1, 4:5))
  ans_obtained <- format_timevar(data = data, timevar = "tm")
  ans_expected <- data.frame(tm = factor(c(1, 4:5), levels = 1:5))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'format_timevar' works when time variable is a factor", {
  data <- data.frame(tm = factor(1:5, levels = 5:1))
  ans_obtained <- format_timevar(data = data, timevar = "tm")
  expect_identical(ans_obtained, data)
})


## 'get_indvar_final' ---------------------------------------------------------

test_that("get_indvar_final' works, no by, numeric indvar", {
  set.seed(0)
  data <- data.frame(time = 1:5, y = rnorm(5, mean = 1:5))
  fit <- fit_ts(data,
                indvar = "y",
                spec_ts = DampedTrend())
  n_draw(fit) <- 5
  ans_obtained <- get_indvar_final(data = data,
                                  indvar = "y",
                                  timevar = "time",
                                  byvar = character(),
                                  n_draw = 5)
  ans_expected <- data.frame(.indvar_final = 1)
  ans_expected$.indvar_final <- list(rep(data$y[5], times = 5))
  expect_identical(ans_obtained, ans_expected)
})

test_that("get_indvar_final' works, has by, list invar", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     sex = rep(c("f", "m"), each = 5))
  data$y <- replicate(n = 10, rnorm(5), simplify = FALSE)
  fit <- fit_ts(data,
                indvar = "y",
                byvar = "sex",
                spec_ts = DampedTrend())
  ans_obtained <- get_indvar_final(data = data,
                                   indvar = "y",
                                   timevar = "time",
                                   byvar = "sex",
                                   n_draw = 5)
  ans_expected <- data.frame(sex = c("f", "m"))
  ans_expected$.indvar_final <- data$y[c(5, 10)]
  expect_identical(ans_obtained, ans_expected)
})


## 'make_benchmarks' ----------------------------------------------------------

test_that("make_benchmarks' works, no by", {
  set.seed(0)
  mean_sd_bench <- data.frame(.mean_sd_bench = NA)
  mean_sd_bench$.mean_sd_bench <- list(list(mean = c(NA, 7, NA, 5),
                                            sd = c(NA, 1, NA, 2.1)))
  indvar_final <- data.frame(.indvar_final = NA)
  indvar_final$.indvar_final <- list(rep(8.2, 3))
  method_spline <- "natural"
  byvar <- character()
  ans_obtained <- make_benchmarks(mean_sd_bench = mean_sd_bench,
                                  indvar_final = indvar_final,
                                  method_spline = method_spline,
                                  byvar = byvar)
  ans_expected <- data.frame(.benchmarks = NA)
  ans_expected$.benchmarks <- list(make_benchmarks_inner(mean_sd_bench = mean_sd_bench[[1]][[1]],
                                                         indvar_final = indvar_final[[1]][[1]],
                                                         method_spline = "natural"))
  expect_identical(ans_obtained, ans_expected)
})

test_that("make_benchmarks' works, has by", {
  set.seed(0)
  mean_sd_bench <- data.frame(sex = c("f", "m"))
  mean_sd_bench$.mean_sd_bench <- list(list(mean = c(NA, 7, NA, 5),
                                            sd = c(NA, 1, NA, 2.1)),
                                       list(mean = c(NA, 3, NA, 4.2),
                                            sd = c(NA, 0.5, NA, 1.3)))
  indvar_final <- data.frame(sex = c("m", "f"))
  indvar_final$.indvar_final <- list(rep(8.2, 3),
                                     rep(7.2, 3))
  method_spline <- "natural"
  byvar <- "sex"
  ans_obtained <- make_benchmarks(mean_sd_bench = mean_sd_bench,
                                  indvar_final = indvar_final,
                                  method_spline = method_spline,
                                  byvar = byvar)
  ans_expected <- data.frame(sex = c("f", "m"))
  ans_expected$.benchmarks <- list(make_benchmarks_inner(mean_sd_bench = mean_sd_bench[[2]][[1]],
                                                         indvar_final = indvar_final[[2]][[2]],
                                                         method_spline = "natural"),
                                   make_benchmarks_inner(mean_sd_bench = mean_sd_bench[[2]][[2]],
                                                         indvar_final = indvar_final[[2]][[1]],
                                                         method_spline = "natural"))
  expect_identical(ans_obtained, ans_expected)
})



## 'make_benchmarks_inner' ----------------------------------------------------

test_that("'make_benchmarks_inner' works with valid inputs", {
  mean_sd_bench <- list(mean = c(2.2, NA, NA, 4.8),
                        sd = c(0.5, NA, NA, 3))
  indvar_final <- c(1, 1.1)
  method_spline <- "natural"
  ans_obtained <- make_benchmarks_inner(mean_sd_bench = mean_sd_bench,
                                        indvar_final = indvar_final,
                                        method_spline = method_spline)
  sd <- splinefun(x = c(1, 2, 5),
                  y = c(0, 0.5, 3),
                  method = "natural")(2:5)
  ans_expected <- list(list(mean = splinefun(x = c(1, 2, 5),
                                             y = c(1, 2.2, 4.8),
                                             method = "natural")(2:5),
                            sd = sd),
                       list(mean = splinefun(x = c(1, 2, 5),
                                             y = c(1.1, 2.2, 4.8),
                                             method = "natural")(2:5),
                            sd = sd))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_i_bench' -------------------------------------------------------------

test_that("make_i_bench' works with valid inputs - no 'by'", {
  by_fitted <- as.data.frame(matrix(nr = 1, nc = 0))
  by_bench <- as.data.frame(matrix(nr = 1, nc = 0))
  ans_obtained <- make_i_bench(by_fitted = by_fitted, by_bench = by_bench)
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)
})

test_that("make_i_bench' works with valid inputs - has 'by'", {
  by_fitted <- expand.grid(a = 1:3, b = 1:2, c = 1:2)
  by_bench <- expand.grid(a = 1:4, c = 1:2)
  ans_obtained <- make_i_bench(by_fitted = by_fitted, by_bench = by_bench)
  ans_expected <- c(1:3, 1:3, 5:7, 5:7)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_mean_sd_bench' -------------------------------------------------------

test_that("make_mean_sd_bench' works - no by", {
  data <- data.frame(time = 1:5, y = 101:105)
  fitted <- fit_ts(data, indvar = "y")
  bench <- Benchmarks(data.frame(time = 8, q50 = 7, q90 = 10))
  ans_obtained <- make_mean_sd_bench(spec_bench = bench,
                                     timevar = "time",
                                     byvar = character(),
                                     by = fitted$by,
                                     time_labels = 6:8,
                                     log = FALSE)
  .mean_sd <- list(list(mean = c(NA, NA, 7), sd = c(NA, NA, 3 / qnorm(0.9))))
  ans_expected <- data.frame(.mean_sd_bench = NA)
  ans_expected$.mean_sd_bench <- .mean_sd
  expect_identical(ans_obtained, ans_expected)
})

test_that("make_mean_sd_bench' works - has by", {
  data <- data.frame(time = c(1:5, 1:5),
                     sex = rep(c("F", "M"), each = 5),
                     y = c(2:6, 0:4))
  fitted <- fit_ts(data, indvar = "y", byvar = "sex")
  bench <- Benchmarks(data.frame(sex = c("F", "M", "M"),
                                 time = c(8, 7, 8),
                                 q50 = c(7, 4, 3),
                                 q90 = c(8, 6, 7)))
  ans_obtained <- make_mean_sd_bench(spec_bench = bench,
                                     timevar = "time",
                                     byvar = "sex",
                                     by = fitted$by,
                                     time_labels = 6:8,
                                     log = TRUE)
  .mean_sd <- list(list(mean = c(NA, NA, log(7)), sd = c(NA, NA, (log(8) - log(7)) / qnorm(0.9))),
                   list(mean = c(NA, log(4), log(3)), sd = c(NA, (log(6) - log(4)) / qnorm(0.9), 
                   (log(7) - log(3)) / qnorm(0.9))))
  ans_expected <- fitted$by
  ans_expected$.mean_sd_bench <- .mean_sd
  expect_identical(ans_obtained, ans_expected)
})


## 'make_mean_sd_bench_inner' -------------------------------------------------

test_that("make_mean_sd_bench_inner' works with log = FALSE", {
  x <- data.frame(tm = 2001:2005,
                  q50 = 1:5,
                  q90 = 2:6)
  ans_obtained <- make_mean_sd_bench_inner(x, log = FALSE)
  ans_expected <- list(mean = 1:5, sd = rep(1 / qnorm(0.9), times = 5))
  expect_identical(ans_obtained, ans_expected)
})

test_that("make_mean_sd_bench_inner' works with log = TRUE", {
  x <- data.frame(tm = 2001:2005,
                  q50 = 1:5,
                  q90 = 2:6)
  ans_obtained <- make_mean_sd_bench_inner(x, log = TRUE)
  ans_expected <- list(mean = log(1:5), sd = (log(2:6) - log(1:5)) / qnorm(0.9))
  expect_identical(ans_obtained, ans_expected)
})


## 'prepare_inputs_fit' -------------------------------------------------------

test_that("prepare_inputs_fit' works with no 'by' variable, numeric indicator", {
  data <- data.frame(time = 1:5, y = 101:105)
  ans <- prepare_inputs_fit(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = character(),
                            log = FALSE)
  ans_obtained <- ans$val
  ans_expected <- list(list(as.double(101:105)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("prepare_inputs_fit' works with no 'by' variable, list indicator", {
  data <- data.frame(time = 1:2)
  data$y <- list(c(101, 102), c(103, 104))
  ans <- prepare_inputs_fit(data = data,
                            indvar = "y",
                            timevar = "time",
                            byvar = character(),
                            log = TRUE)
  ans_obtained <- ans$val
  ans_expected <- list(list(log(c(101, 103)),
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
