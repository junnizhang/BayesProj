
smoothed_withage <- smooth_agetime(nevent_df = nz_divorces,
                                   py_df = nz_population)
smoothed_total <- total_rate(smoothed_withage)

model <- ts_model(smoothed_total,
                  spec = DampedTrend(scale_level = 0.01))
jz_bench <- tibble(time = c(2005, 2006),
                   mean = c(NA, 3.4),
                   sd = c(NA, 2.3))
proj <- ts_proj(model,
                bench = jz_bench)
comp_mod <- components(model)
aug_mod <- augment(model)
                       
comp_proj <- components(proj)
aug_proj <- augment(proj)




