#include <stdlib.h>
#include <R_ext/Rdynload.h>

extern "C" {

  const static R_CallMethodDef R_CallDef[] = {
    TMB_CALLDEFS,
    {NULL, NULL, 0}
  };

  void R_init_BayesProj(DllInfo *dll) {
    R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
#ifdef TMB_CCALLABLES
    TMB_CCALLABLES("BayesProj");
#endif
  }

}
