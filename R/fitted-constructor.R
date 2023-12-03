

new_BayesProj_fitted <- function(data,
                                 indvar,
                                 timevar,
                                 byvar,
                                 spec_ts,
                                 fitted,
                                 by,
                                 labels_time,
                                 log) {
  n_draw <- length(fitted[[1L]])
  if (identical(n_draw, 1L))
    n_draw <- 1000L
  ans <- list(data = data,
              indvar = indvar,
              timevar = timevar,
              byvar = byvar,
              spec_ts = spec_ts,
              fitted = fitted,
              by = by,
              labels_time = labels_time,
              log = log,
              n_draw = n_draw)
  class(ans) <- "BayesProj_fitted"
  ans
}
  
