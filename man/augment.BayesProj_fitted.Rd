% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitted-methods.R
\name{augment.BayesProj_fitted}
\alias{augment.BayesProj_fitted}
\title{Smoothed Estimates of Indicator, Combined with Original Data}
\usage{
\method{augment}{BayesProj_fitted}(x, interval = 0.95, ...)
}
\arguments{
\item{x}{Results from a call to \code{\link[=fit_ts]{fit_ts()}}.}

\item{interval}{Width of credible intervals.
A number between 0 and 1. Default is \code{0.95}.}

\item{...}{Not currently used.}
}
\value{
A \link[tibble:tibble-package]{tibble},
constructed by adding four new columns to \code{data}:
\itemize{
\item \code{.fitted}: Point estimates (posterior medians) of rates.
\item \code{.lower}, \code{.upper}: Lower and upper bounds of
credible intervals specified by \code{interval}.
\item \code{.probability}: A list column with all draws
from the posterior distribution.
}
}
\description{
Given the output from a call to \code{\link[=fit_ts]{fit_ts()}},
extract smoothed values for the demographic indicator,
merged with the original data.
}
\details{
If \code{log} is \code{FALSE} in the original call to
\code{\link[=fit_ts]{fit_ts()}} (which is the default), then the
smoothed values are derived from the \code{level}
parameter. If \code{log} is \code{TRUE}, then the
smoothed values are derived from \code{exp(level)}.
}
\examples{
library(tibble)
data <- tribble(~time, ~val,
                2015,  4.3,
                2016,  4.1,
                2020,  3.5,
                2022,  3.6)
fit <- fit_ts(data, indvar = "val")
augment(fit)
}
\seealso{
\itemize{
\item \code{\link[=fit_ts]{fit_ts()}} to fit a time series model.
\item \code{\link[=components.BayesProj_fitted]{components()}}
extracts parameter and hyper-parameter estimates.
\item \code{\link[=n_draw<-]{n_draw()}} sets the default number
of draws from the posterior distribution.
}
}
