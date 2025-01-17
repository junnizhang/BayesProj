
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BayesProj

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/junnizhang/BayesProj/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/junnizhang/BayesProj/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Using Bayesian methods, project demographic indicators. Optionally,
incorporate expert judgment into the projections by specifying
benchmarks.

**BayesProj** is part of a larger project to extend the
[ProFamy](https://link.springer.com/article/10.1007/s42379-024-00171-6)
household projection method.

Work on **BayesProj** has been supported by funding from the Chinese
Ministry of Science and Technology.

## Installation

Install the development version of BayesProj from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("junnizhang/BayesProj")
```

## Example

``` r
library(BayesRates)
library(BayesProj)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
```

Smooth Chinese data on divorce rates

``` r
res <- smooth_agetime(nevent_df = cn_divorces,
                      py_df = cn_population,
                      spec_time = TimeFixed(),
                      byvar = "sex")
```

Calculate the total divorce rate

``` r
TDR <- total_rate(res)
```

Extract results

``` r
data <- TDR |>
  select(sex, time, TDR = .probability)
```

Fit a time series model to the historical estimates

``` r
fitted <- fit_ts(data, 
                 indvar = "TDR",
                 byvar = "sex")
```

Project future values

``` r
projected <- project_ts(fitted, 
                        time_labels = 2019:2028)
```

Extract historical and projected values and plot them

``` r
vals <- composite(projected)

ggplot(vals, aes(x = time, y = TDR)) +
  facet_wrap(vars(sex)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), 
              fill = "salmon") +
  geom_line(col = "darkred", 
            linewidth = 0.5)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

Create benchmarks

``` r
bench <- Benchmarks(data.frame(time = c(2028, 2028),
                               sex = c("Female", "Male"),
                               q50 = c(0.15, 0.25),
                               q90 = c(0.2, 0.3)))
```

Re-do the projection, with benchmarks

``` r
projected_bench <- project_ts(fitted, 
                              time_labels = 2019:2028,
                  spec_bench = bench)

vals_bench <- composite(projected_bench)

ggplot(vals_bench, aes(x = time, y = TDR)) +
  facet_wrap(vars(sex)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), 
              fill = "salmon") +
  geom_line(col = "darkred", 
            linewidth = 0.5)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />
