#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _IncDTW_BACKTRACK_cpp(SEXP);
extern SEXP _IncDTW_BACKTRACK2II_cpp(SEXP, SEXP);
extern SEXP _IncDTW_BACKTRACK2IN_cpp(SEXP, SEXP);
extern SEXP _IncDTW_cpp_cm(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_diffm(SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec(SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_cm(SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_cm_ws_ea(SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_ea(SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_inc(SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_inc_mv(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_inc_mv_ws(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_inc_ws(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_mv(SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_mv_ws_ea(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_v32(SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_ws(SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_dtw2vec_ws_ea(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_norm01(SEXP, SEXP, SEXP);
extern SEXP _IncDTW_cpp_znorm(SEXP, SEXP, SEXP);
extern SEXP _IncDTW_GCM_cpp(SEXP, SEXP);
extern SEXP _IncDTW_GCM_Sakoe_cpp(SEXP, SEXP, SEXP);
extern SEXP _IncDTW_IGCM_cpp(SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_IGCM_Sakoe_cpp(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_normmat(SEXP);
extern SEXP _IncDTW_parallel_dm_dtw(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_parallel_dm_dtw_mv(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_parallel_dv_dtw(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _IncDTW_parallel_dv_dtw_mv(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_IncDTW_BACKTRACK_cpp",         (DL_FUNC) &_IncDTW_BACKTRACK_cpp,         1},
    {"_IncDTW_BACKTRACK2II_cpp",      (DL_FUNC) &_IncDTW_BACKTRACK2II_cpp,      2},
    {"_IncDTW_BACKTRACK2IN_cpp",      (DL_FUNC) &_IncDTW_BACKTRACK2IN_cpp,      2},
    {"_IncDTW_cpp_cm",                (DL_FUNC) &_IncDTW_cpp_cm,                5},
    {"_IncDTW_cpp_diffm",             (DL_FUNC) &_IncDTW_cpp_diffm,             4},
    {"_IncDTW_cpp_dtw2vec",           (DL_FUNC) &_IncDTW_cpp_dtw2vec,           3},
    {"_IncDTW_cpp_dtw2vec_cm",        (DL_FUNC) &_IncDTW_cpp_dtw2vec_cm,        2},
    {"_IncDTW_cpp_dtw2vec_cm_ws_ea",  (DL_FUNC) &_IncDTW_cpp_dtw2vec_cm_ws_ea,  4},
    {"_IncDTW_cpp_dtw2vec_ea",        (DL_FUNC) &_IncDTW_cpp_dtw2vec_ea,        4},
    {"_IncDTW_cpp_dtw2vec_inc",       (DL_FUNC) &_IncDTW_cpp_dtw2vec_inc,       4},
    {"_IncDTW_cpp_dtw2vec_inc_mv",    (DL_FUNC) &_IncDTW_cpp_dtw2vec_inc_mv,    5},
    {"_IncDTW_cpp_dtw2vec_inc_mv_ws", (DL_FUNC) &_IncDTW_cpp_dtw2vec_inc_mv_ws, 7},
    {"_IncDTW_cpp_dtw2vec_inc_ws",    (DL_FUNC) &_IncDTW_cpp_dtw2vec_inc_ws,    6},
    {"_IncDTW_cpp_dtw2vec_mv",        (DL_FUNC) &_IncDTW_cpp_dtw2vec_mv,        4},
    {"_IncDTW_cpp_dtw2vec_mv_ws_ea",  (DL_FUNC) &_IncDTW_cpp_dtw2vec_mv_ws_ea,  6},
    {"_IncDTW_cpp_dtw2vec_v32",       (DL_FUNC) &_IncDTW_cpp_dtw2vec_v32,       2},
    {"_IncDTW_cpp_dtw2vec_ws",        (DL_FUNC) &_IncDTW_cpp_dtw2vec_ws,        4},
    {"_IncDTW_cpp_dtw2vec_ws_ea",     (DL_FUNC) &_IncDTW_cpp_dtw2vec_ws_ea,     5},
    {"_IncDTW_cpp_norm01",            (DL_FUNC) &_IncDTW_cpp_norm01,            3},
    {"_IncDTW_cpp_znorm",             (DL_FUNC) &_IncDTW_cpp_znorm,             3},
    {"_IncDTW_GCM_cpp",               (DL_FUNC) &_IncDTW_GCM_cpp,               2},
    {"_IncDTW_GCM_Sakoe_cpp",         (DL_FUNC) &_IncDTW_GCM_Sakoe_cpp,         3},
    {"_IncDTW_IGCM_cpp",              (DL_FUNC) &_IncDTW_IGCM_cpp,              4},
    {"_IncDTW_IGCM_Sakoe_cpp",        (DL_FUNC) &_IncDTW_IGCM_Sakoe_cpp,        5},
    {"_IncDTW_normmat",               (DL_FUNC) &_IncDTW_normmat,               1},
    {"_IncDTW_parallel_dm_dtw",       (DL_FUNC) &_IncDTW_parallel_dm_dtw,       7},
    {"_IncDTW_parallel_dm_dtw_mv",    (DL_FUNC) &_IncDTW_parallel_dm_dtw_mv,    8},
    {"_IncDTW_parallel_dv_dtw",       (DL_FUNC) &_IncDTW_parallel_dv_dtw,       6},
    {"_IncDTW_parallel_dv_dtw_mv",    (DL_FUNC) &_IncDTW_parallel_dv_dtw_mv,    7},
    {NULL, NULL, 0}
};

void R_init_IncDTW(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
