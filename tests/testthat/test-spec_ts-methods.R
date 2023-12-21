
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


## 'get_par_final' ------------------------------------------------------------

test_that("get_par_final works with damped trend, no 'by'", {
  set.seed(0)
  data <- data.frame(time = 1:5)
  data$y <- replicate(5, rnorm(10), simplify = FALSE)
  fitted <- fit_ts(data, indvar = "y")
  draws_post <- draw_post_fit(fitted)
  ans_obtained <- get_par_final(spec_ts = fitted$spec_ts,
                                draws = draws_post,
                                timevar = "time",
                                byvar = character())
  ans_expected <- data.frame(.par_final = NA)
  ans_expected$.par_final <- list(lapply(1:10, function(i) c(level = draws_post$level$.probability[[5]][i],
                                                             trend = draws_post$trend$.probability[[5]][i])))
  expect_identical(ans_obtained, ans_expected)
})

test_that("get_par_final works with damped trend, has 'by'", {
  set.seed(0)
  data <- data.frame(time = rep(1:5, 2),
                     reg = rep(c("a", "b"), each = 5))
  data$y <- replicate(10, rnorm(10), simplify = FALSE)
  fitted <- fit_ts(data, indvar = "y", byvar = "reg")
  draws_post <- draw_post_fit(fitted)
  ans_obtained <- get_par_final(spec_ts = fitted$spec_ts,
                                draws = draws_post,
                                timevar = "time",
                                byvar = "reg")
  ans_expected <- data.frame(reg = c("a", "b"), .par_final = 1:2)
  ans_expected$.par_final <- list(lapply(1:10, function(i) c(level = draws_post$level$.probability[[5]][i],
                                                             trend = draws_post$trend$.probability[[5]][i])),
                                  lapply(1:10, function(i) c(level = draws_post$level$.probability[[10]][i],
                                                             trend = draws_post$trend$.probability[[10]][i])))
  expect_identical(ans_obtained, ans_expected)
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


## 'make_labels_proj' ---------------------------------------------------------

test_that("'make_labels_proj' works with damped trend", {
  spec <- DampedTrend()
  labels_time_project <- 2000:2002
  ans_obtained <- make_labels_proj(spec = spec, labels_time_project = labels_time_project)
  ans_expected <- c(2000:2002,
                    2000:2002,
                    2000:2002,
                    "sd_obs", "sd_level", "sd_trend", "damp")
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


## 'make_mean_proj', 'make_prec_proj' -----------------------------------------

test_that("'make_mean_proj', 'make_prec_proj' work with damped trend, no benchmarks", {
  set.seed(0)
  level_final <- 2
  trend_final <- 0.3
  sd_y <- 0.3
  sd_level <- 0.4
  sd_trend <- 0.2
  damp <- 0.9
  n_draw <- 1000
  n_time <- 10
  ## simulate directly
  ans_direct <- array(dim = c(n_draw, n_time, 3))
  for (i_draw in seq_len(n_draw)) {
    trend <- numeric(n_time)
    level <- numeric(n_time)
    level[1] <- level_final + trend_final + rnorm(1, sd = sd_level)
    trend[1] <- damp * trend_final + rnorm(1, sd = sd_trend)
    for (i in seq_len(n_time - 1)) {
      level[i + 1] = level[i] + trend[i] + rnorm(1, sd = sd_level)
      trend[i + 1] = damp * trend[i] + rnorm(1, sd = sd_trend)
    }
    y <- rnorm(n = 10, mean = level, sd = sd_y)
    ans_direct[i_draw, , 1]  <- y
    ans_direct[i_draw, , 2]  <- level
    ans_direct[i_draw, , 3]  <- trend
  }
  ## simulate via mvn draw
  spec <- DampedTrend()
  par_final <- c(level_final, trend_final)
  hyper <- c(sd_y, sd_level, sd_trend, damp)
  mean_bench <- rep(0, n_time)
  sd_bench <- rep(Inf, n_time)
  prec_proj <- make_prec_proj(spec = spec,
                              hyper = hyper,
                              sd_bench = sd_bench)
  mean_proj <- make_mean_proj(spec = spec,
                              par_final = par_final,
                              hyper = hyper,
                              mean_bench = mean_bench,
                              sd_bench = sd_bench,
                              prec_proj = prec_proj)
  ans_mvn <- array(dim = c(n_draw, n_time, 3))
  for (i_draw in seq_len(n_draw))
    ans_mvn[i_draw, , ] <- rmvn(n = 1, mean = mean_proj, prec = prec_proj)
  expect_equal(apply(ans_direct, 2:3, mean),
               apply(ans_mvn, 2:3, mean),
               tolerance = 0.05)
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

test_that("'make_vname_fit' works with damped trend", {
  spec <- DampedTrend()
  timevar <- "time"
  ans_obtained <- make_vname_fit(spec = spec, timevar = timevar)
  ans_expected <- c("time", "time", "hyper")
  expect_identical(ans_obtained, ans_expected)
})


## 'make_vname_proj' -----------------------------------------------------------

test_that("'make_vname_proj' works with damped trend", {
  spec <- DampedTrend()
  timevar <- "time"
  ans_obtained <- make_vname_proj(spec = spec, timevar = timevar)
  ans_expected <- c("time", "time", "time", "hyper")
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


## 'make_what_proj' ------------------------------------------------------------

test_that("'make_what_proj' works with damped trend", {
  spec <- DampedTrend()
  indvar <- "tfr"
  labels_time_project <- 2000:2002
  ans_obtained <- make_what_proj(spec = spec,
                                 indvar = indvar,
                                 labels_time_project = labels_time_project)
  ans_expected <- factor(rep(c("tfr", "level", "trend", "hyper"),
                             times = c(3, 3, 3, 4)),
                         levels = c("tfr", "level", "trend", "hyper"))
  expect_identical(ans_obtained, ans_expected)
})
