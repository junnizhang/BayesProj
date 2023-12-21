
## When converting to S7 class, make this inherit from BayesProj_fitted

new_BayesProj_proj <- function(data,
                               indvar,
                               timevar,
                               byvar,
                               spec_ts,
                               fitted,
                               by,
                               labels_time,
                               log,
                               n_draw = n_draw,
                               labels_time_project,
                               benchmarks,
                               method_spline) {
  ans <- list(data = data,
              indvar = indvar,
              timevar = timevar,
              byvar = byvar,
              spec_ts = spec_ts,
              fitted = fitted,
              by = by,
              labels_time = labels_time,
              log = log,
              n_draw = n_draw,
              labels_time_project = labels_time_project,
              benchmarks = benchmarks,
              method_spline = method_spline)
  class(ans) <- "BayesProj_proj"
  ans
}
