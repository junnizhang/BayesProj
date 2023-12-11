
new_BayesProj_proj <- function(data,
                               indvar,
                               timevar,
                               byvar,
                               spec_ts,
                               fitted,
                               by,
                               labels_time_fitted,
                               log,
                               labels_time_project,
                               mean_bench,
                               sd_bench) {
  ans <- list(data = data,
              indvar = indvar,
              timevar = timevar,
              byvar = byvar,
              spec_ts = spec_ts,
              fitted = fitted,
              by = by,
              labels_time_fitted = labels_time_fitted,
              log = log,
              labels_time_project = labels_time_project,
              mean_bench = mean_bench,
              sd_bench = sd_bench)
  class(ans) <- "BayesProj_proj"
  ans
}
