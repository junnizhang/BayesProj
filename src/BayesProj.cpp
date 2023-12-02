
#include <TMB.hpp>
#include "init.h"

using namespace Eigen;
using namespace density;

// Objective function ---------------------------------------------------------

template<class Type>
Type objective_function<Type>::operator() ()
{

  // inputs -------------------------------------------------------------------

  DATA_VECTOR(y);
  DATA_IVECTOR(is_in_lik);
  DATA_STRING(class_spec);
  DATA_VECTOR(consts);

  PARAMETER_VECTOR(par);
  PARAMETER_VECTOR(hyper);

  // derived quantities -------------------------------------------------------

  int T = y.size();
  
  // log posterior ------------------------------------------------------------
  
  Type ans = 0;

  if (class_spec == "dampedtrend") {
    
    // extract values

    Type sd_level_init = consts[0];
    Type sd_trend_init = consts[1];
    Type scale_sd_y = consts[2];
    Type scale_sd_level = consts[3];
    Type scale_sd_trend = consts[4];
    Type damp_min = consts[5];
    Type damp_max = consts[6];
    vector<Type> level = par;
    vector<Type> trend = hyper.segment(0, T);
    Type log_sd_y = hyper[T];
    Type log_sd_level = hyper[T + 1];
    Type log_sd_trend = hyper[T + 2];
    Type logit_damp = hyper[T + 3];

    // create new values
    
    Type sd_y = exp(log_sd_y);
    Type sd_level = exp(log_sd_level);
    Type sd_trend = exp(log_sd_trend);
    Type damp_raw = exp(logit_damp) / (1 + exp(logit_damp));
    Type damp = damp_min + (damp_max - damp_min) * damp_raw;
    
    // contributions to negative log posterior
    
    ans -= dnorm(sd_y, Type(0), scale_sd_y, true) + log_sd_y;
    ans -= dnorm(sd_level, Type(0), scale_sd_level, true) + log_sd_level;
    ans -= dnorm(sd_trend, Type(0), scale_sd_trend, true) + log_sd_trend;
    ans -= dbeta(damp_raw, Type(2), Type(2), true) + log(damp_raw) + log(1 - damp_raw);
    ans -= dnorm(level[0], Type(0), sd_level_init, true);
    ans -= dnorm(trend[0], Type(0), sd_trend_init, true);
    for (int t = 0; t < T; t++) {
      if (is_in_lik[t])
	ans -= dnorm(y[t], level[t], sd_y, true);
    }
    for (int t = 1; t < T; t++) {
      ans -= dnorm(level[t], level[t-1] + trend[t-1], sd_level, true);
      ans -= dnorm(trend[t], damp * trend[t-1], sd_trend, true);
    }

  }    
      
  return ans; 
}
