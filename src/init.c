#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP IncDTW_BACKTRACK_cpp(SEXP);
extern SEXP IncDTW_GCM_cpp(SEXP);
extern SEXP IncDTW_GCM_Sakoe_cpp(SEXP, SEXP);
extern SEXP IncDTW_IGCM_cpp(SEXP, SEXP, SEXP);
extern SEXP IncDTW_IGCM_Sakoe_cpp(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"IncDTW_BACKTRACK_cpp",  (DL_FUNC) &IncDTW_BACKTRACK_cpp,  1},
    {"IncDTW_GCM_cpp",        (DL_FUNC) &IncDTW_GCM_cpp,        1},
    {"IncDTW_GCM_Sakoe_cpp",  (DL_FUNC) &IncDTW_GCM_Sakoe_cpp,  2},
    {"IncDTW_IGCM_cpp",       (DL_FUNC) &IncDTW_IGCM_cpp,       3},
    {"IncDTW_IGCM_Sakoe_cpp", (DL_FUNC) &IncDTW_IGCM_Sakoe_cpp, 4},
    {NULL, NULL, 0}
};

void R_init_IncDTW(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
