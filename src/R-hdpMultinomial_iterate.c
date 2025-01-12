#include <stdlib.h>
#include "R-hdpMultinomial_iterate.h"

SEXP hdpMultinomial_iterate(SEXP hdpin, SEXP numiter, SEXP doconparam, SEXP dolik, SEXP dodebug)
{
  int ni, docp, dl;

  GetRNGstate();

  ni = asInteger(numiter);
  docp = asInteger(doconparam);
  dl = asInteger(dolik);

  hdpx_debug = asInteger(dodebug);

  HDP *hdp = rReadHDP(hdpin);

  SEXP LIK = PROTECT(allocVector(REALSXP, ni));

  rdebug0(1,"Running hdpMultinomial_iterate.\n");
  hdp_iterate(hdp, REAL(LIK), ni, docp, dl);
  rdebug0(1,"Finished hdpMultinomial_iterate.\n");

  SEXP hdpout = PROTECT(duplicate(hdpin));
  rWriteHDP(hdpout,hdp);

  SEXP result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, hdpout);
  SET_VECTOR_ELT(result, 1, LIK);

  /* In https://github.com/nicolaroberts/hdp UNPROTECT(3) was here,
   * but the following PutRNGstate() could trigger a garbage
   * collection, invalidating UNPROTECTed pointers in result.
   */

  PutRNGstate();

  UNPROTECT(3);

  return result;
}


