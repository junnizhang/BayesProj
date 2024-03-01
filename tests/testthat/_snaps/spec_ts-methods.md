# 'print' works

    Code
      print(DampedTrend())
    Output
      < Object of class "BayesProj_spec_ts_dampedtrend" >
            scale_obs: NULL
          scale_level: NULL
             damp_min: 0.8
             damp_max: 0.98

---

    Code
      print(DampedTrend(scale_obs = 3, scale_level = 0.1))
    Output
      < Object of class "BayesProj_spec_ts_dampedtrend" >
            scale_obs: 3
          scale_level: 0.1
             damp_min: 0.8
             damp_max: 0.98

---

    Code
      print(AR1())
    Output
      < Object of class "BayesProj_spec_ts_ar1" >
            scale_obs: NULL
          scale_level: NULL
             damp_min: 0.8
             damp_max: 0.98

---

    Code
      print(AR1(scale_obs = 3, scale_level = 0.1))
    Output
      < Object of class "BayesProj_spec_ts_ar1" >
            scale_obs: 3
          scale_level: 0.1
             damp_min: 0.8
             damp_max: 0.98

