// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// parallel_dm_dtw
Rcpp::NumericVector parallel_dm_dtw(Rcpp::List lot, std::vector<int> ii, std::vector<int> jj, bool normalize, std::string step_pattern, int ws, double threshold);
RcppExport SEXP _IncDTW_parallel_dm_dtw(SEXP lotSEXP, SEXP iiSEXP, SEXP jjSEXP, SEXP normalizeSEXP, SEXP step_patternSEXP, SEXP wsSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type lot(lotSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type ii(iiSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type jj(jjSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(parallel_dm_dtw(lot, ii, jj, normalize, step_pattern, ws, threshold));
    return rcpp_result_gen;
END_RCPP
}
// parallel_dm_dtw_mv
Rcpp::NumericVector parallel_dm_dtw_mv(Rcpp::List lot, std::vector<int> ii, std::vector<int> jj, bool normalize, std::string step_pattern, std::string dist_method, int ws, double threshold);
RcppExport SEXP _IncDTW_parallel_dm_dtw_mv(SEXP lotSEXP, SEXP iiSEXP, SEXP jjSEXP, SEXP normalizeSEXP, SEXP step_patternSEXP, SEXP dist_methodSEXP, SEXP wsSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type lot(lotSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type ii(iiSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type jj(jjSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    Rcpp::traits::input_parameter< std::string >::type dist_method(dist_methodSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(parallel_dm_dtw_mv(lot, ii, jj, normalize, step_pattern, dist_method, ws, threshold));
    return rcpp_result_gen;
END_RCPP
}
// parallel_dv_dtw
Rcpp::NumericVector parallel_dv_dtw(arma::vec Q, Rcpp::List lot, bool normalize, std::string step_pattern, int ws, double threshold);
RcppExport SEXP _IncDTW_parallel_dv_dtw(SEXP QSEXP, SEXP lotSEXP, SEXP normalizeSEXP, SEXP step_patternSEXP, SEXP wsSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type Q(QSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type lot(lotSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(parallel_dv_dtw(Q, lot, normalize, step_pattern, ws, threshold));
    return rcpp_result_gen;
END_RCPP
}
// parallel_dv_dtw_mv
Rcpp::NumericVector parallel_dv_dtw_mv(arma::mat Q, Rcpp::List lot, bool normalize, std::string step_pattern, std::string dist_method, int ws, double threshold);
RcppExport SEXP _IncDTW_parallel_dv_dtw_mv(SEXP QSEXP, SEXP lotSEXP, SEXP normalizeSEXP, SEXP step_patternSEXP, SEXP dist_methodSEXP, SEXP wsSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type Q(QSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type lot(lotSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    Rcpp::traits::input_parameter< std::string >::type dist_method(dist_methodSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(parallel_dv_dtw_mv(Q, lot, normalize, step_pattern, dist_method, ws, threshold));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_cm
double cpp_dtw2vec_cm(Rcpp::NumericMatrix cm, std::string step_pattern);
RcppExport SEXP _IncDTW_cpp_dtw2vec_cm(SEXP cmSEXP, SEXP step_patternSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cm(cmSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_cm(cm, step_pattern));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_cm_inc
List cpp_dtw2vec_cm_inc(Rcpp::NumericVector gcm_lc, Rcpp::NumericMatrix cm, std::string step_pattern);
RcppExport SEXP _IncDTW_cpp_dtw2vec_cm_inc(SEXP gcm_lcSEXP, SEXP cmSEXP, SEXP step_patternSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type gcm_lc(gcm_lcSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cm(cmSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_cm_inc(gcm_lc, cm, step_pattern));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_cm_ws_ea
double cpp_dtw2vec_cm_ws_ea(Rcpp::NumericMatrix cm, std::string step_pattern, int ws, double threshold);
RcppExport SEXP _IncDTW_cpp_dtw2vec_cm_ws_ea(SEXP cmSEXP, SEXP step_patternSEXP, SEXP wsSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cm(cmSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_cm_ws_ea(cm, step_pattern, ws, threshold));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_cm_ws_inc
List cpp_dtw2vec_cm_ws_inc(NumericVector gcm_lc, Rcpp::NumericMatrix cm, std::string step_pattern, int ws, int ny);
RcppExport SEXP _IncDTW_cpp_dtw2vec_cm_ws_inc(SEXP gcm_lcSEXP, SEXP cmSEXP, SEXP step_patternSEXP, SEXP wsSEXP, SEXP nySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type gcm_lc(gcm_lcSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cm(cmSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< int >::type ny(nySEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_cm_ws_inc(gcm_lc, cm, step_pattern, ws, ny));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec
double cpp_dtw2vec(const arma::vec& x, const arma::vec& y, std::string step_pattern);
RcppExport SEXP _IncDTW_cpp_dtw2vec(SEXP xSEXP, SEXP ySEXP, SEXP step_patternSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec(x, y, step_pattern));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_ws
double cpp_dtw2vec_ws(const arma::vec& x, const arma::vec& y, std::string step_pattern, int ws);
RcppExport SEXP _IncDTW_cpp_dtw2vec_ws(SEXP xSEXP, SEXP ySEXP, SEXP step_patternSEXP, SEXP wsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_ws(x, y, step_pattern, ws));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_ea
double cpp_dtw2vec_ea(const arma::vec& x, const arma::vec& y, std::string step_pattern, double threshold);
RcppExport SEXP _IncDTW_cpp_dtw2vec_ea(SEXP xSEXP, SEXP ySEXP, SEXP step_patternSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_ea(x, y, step_pattern, threshold));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_ws_ea
double cpp_dtw2vec_ws_ea(const arma::vec& x, const arma::vec& y, std::string step_pattern, int ws, double threshold);
RcppExport SEXP _IncDTW_cpp_dtw2vec_ws_ea(SEXP xSEXP, SEXP ySEXP, SEXP step_patternSEXP, SEXP wsSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_ws_ea(x, y, step_pattern, ws, threshold));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_mv
double cpp_dtw2vec_mv(const arma::mat& x, const arma::mat& y, std::string step_pattern, std::string dist_method);
RcppExport SEXP _IncDTW_cpp_dtw2vec_mv(SEXP xSEXP, SEXP ySEXP, SEXP step_patternSEXP, SEXP dist_methodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    Rcpp::traits::input_parameter< std::string >::type dist_method(dist_methodSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_mv(x, y, step_pattern, dist_method));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_mv_ws_ea
double cpp_dtw2vec_mv_ws_ea(const arma::mat& x, const arma::mat& y, std::string step_pattern, std::string dist_method, int ws, double threshold);
RcppExport SEXP _IncDTW_cpp_dtw2vec_mv_ws_ea(SEXP xSEXP, SEXP ySEXP, SEXP step_patternSEXP, SEXP dist_methodSEXP, SEXP wsSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    Rcpp::traits::input_parameter< std::string >::type dist_method(dist_methodSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_mv_ws_ea(x, y, step_pattern, dist_method, ws, threshold));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_v32
double cpp_dtw2vec_v32(const arma::vec& x, const arma::vec& y);
RcppExport SEXP _IncDTW_cpp_dtw2vec_v32(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_v32(x, y));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_inc
List cpp_dtw2vec_inc(NumericVector x, NumericVector newObs, NumericVector gcm_lc, std::string step_pattern);
RcppExport SEXP _IncDTW_cpp_dtw2vec_inc(SEXP xSEXP, SEXP newObsSEXP, SEXP gcm_lcSEXP, SEXP step_patternSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type newObs(newObsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type gcm_lc(gcm_lcSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_inc(x, newObs, gcm_lc, step_pattern));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_inc_ws
List cpp_dtw2vec_inc_ws(NumericVector x, NumericVector newObs, NumericVector gcm_lc, int ws, int ny, std::string step_pattern);
RcppExport SEXP _IncDTW_cpp_dtw2vec_inc_ws(SEXP xSEXP, SEXP newObsSEXP, SEXP gcm_lcSEXP, SEXP wsSEXP, SEXP nySEXP, SEXP step_patternSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type newObs(newObsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type gcm_lc(gcm_lcSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< int >::type ny(nySEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_inc_ws(x, newObs, gcm_lc, ws, ny, step_pattern));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_inc_mv
List cpp_dtw2vec_inc_mv(const arma::mat& x, const arma::mat& newObs, const arma::vec& gcm_lc, std::string dist_method, std::string step_pattern);
RcppExport SEXP _IncDTW_cpp_dtw2vec_inc_mv(SEXP xSEXP, SEXP newObsSEXP, SEXP gcm_lcSEXP, SEXP dist_methodSEXP, SEXP step_patternSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type newObs(newObsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type gcm_lc(gcm_lcSEXP);
    Rcpp::traits::input_parameter< std::string >::type dist_method(dist_methodSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_inc_mv(x, newObs, gcm_lc, dist_method, step_pattern));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dtw2vec_inc_mv_ws
List cpp_dtw2vec_inc_mv_ws(const arma::mat& x, const arma::mat& newObs, const arma::vec& gcm_lc, std::string dist_method, int ws, int ny, std::string step_pattern);
RcppExport SEXP _IncDTW_cpp_dtw2vec_inc_mv_ws(SEXP xSEXP, SEXP newObsSEXP, SEXP gcm_lcSEXP, SEXP dist_methodSEXP, SEXP wsSEXP, SEXP nySEXP, SEXP step_patternSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type newObs(newObsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type gcm_lc(gcm_lcSEXP);
    Rcpp::traits::input_parameter< std::string >::type dist_method(dist_methodSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< int >::type ny(nySEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dtw2vec_inc_mv_ws(x, newObs, gcm_lc, dist_method, ws, ny, step_pattern));
    return rcpp_result_gen;
END_RCPP
}
// cpp_znorm
NumericVector cpp_znorm(NumericVector x, Rcpp::Nullable< Rcpp::NumericVector > mu_in, Rcpp::Nullable< Rcpp::NumericVector > sd_in);
RcppExport SEXP _IncDTW_cpp_znorm(SEXP xSEXP, SEXP mu_inSEXP, SEXP sd_inSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable< Rcpp::NumericVector > >::type mu_in(mu_inSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable< Rcpp::NumericVector > >::type sd_in(sd_inSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_znorm(x, mu_in, sd_in));
    return rcpp_result_gen;
END_RCPP
}
// cpp_norm01
NumericVector cpp_norm01(NumericVector x, Rcpp::Nullable< Rcpp::NumericVector > min_in, Rcpp::Nullable< Rcpp::NumericVector > max_in);
RcppExport SEXP _IncDTW_cpp_norm01(SEXP xSEXP, SEXP min_inSEXP, SEXP max_inSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable< Rcpp::NumericVector > >::type min_in(min_inSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable< Rcpp::NumericVector > >::type max_in(max_inSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_norm01(x, min_in, max_in));
    return rcpp_result_gen;
END_RCPP
}
// cpp_cm
NumericMatrix cpp_cm(const arma::mat& x, const arma::mat& y, std::string dist_method, int ws, int nPrevObs);
RcppExport SEXP _IncDTW_cpp_cm(SEXP xSEXP, SEXP ySEXP, SEXP dist_methodSEXP, SEXP wsSEXP, SEXP nPrevObsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type dist_method(dist_methodSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< int >::type nPrevObs(nPrevObsSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_cm(x, y, dist_method, ws, nPrevObs));
    return rcpp_result_gen;
END_RCPP
}
// cpp_diffm
NumericMatrix cpp_diffm(const NumericVector& x, const NumericVector& y, int ws, int nPrevObs);
RcppExport SEXP _IncDTW_cpp_diffm(SEXP xSEXP, SEXP ySEXP, SEXP wsSEXP, SEXP nPrevObsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< int >::type nPrevObs(nPrevObsSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_diffm(x, y, ws, nPrevObs));
    return rcpp_result_gen;
END_RCPP
}
// GCM_Sakoe_cpp
List GCM_Sakoe_cpp(Rcpp::NumericMatrix cM, int ws, std::string step_pattern);
RcppExport SEXP _IncDTW_GCM_Sakoe_cpp(SEXP cMSEXP, SEXP wsSEXP, SEXP step_patternSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cM(cMSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    rcpp_result_gen = Rcpp::wrap(GCM_Sakoe_cpp(cM, ws, step_pattern));
    return rcpp_result_gen;
END_RCPP
}
// IGCM_Sakoe_cpp
List IGCM_Sakoe_cpp(Rcpp::NumericMatrix gcmN, Rcpp::IntegerMatrix dmN, Rcpp::NumericMatrix cmN, int ws, std::string step_pattern);
RcppExport SEXP _IncDTW_IGCM_Sakoe_cpp(SEXP gcmNSEXP, SEXP dmNSEXP, SEXP cmNSEXP, SEXP wsSEXP, SEXP step_patternSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type gcmN(gcmNSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type dmN(dmNSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cmN(cmNSEXP);
    Rcpp::traits::input_parameter< int >::type ws(wsSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    rcpp_result_gen = Rcpp::wrap(IGCM_Sakoe_cpp(gcmN, dmN, cmN, ws, step_pattern));
    return rcpp_result_gen;
END_RCPP
}
// GCM_cpp
List GCM_cpp(Rcpp::NumericMatrix cM, std::string step_pattern);
RcppExport SEXP _IncDTW_GCM_cpp(SEXP cMSEXP, SEXP step_patternSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cM(cMSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    rcpp_result_gen = Rcpp::wrap(GCM_cpp(cM, step_pattern));
    return rcpp_result_gen;
END_RCPP
}
// IGCM_cpp
List IGCM_cpp(Rcpp::NumericMatrix gcmN, Rcpp::IntegerMatrix dmN, Rcpp::NumericMatrix cmN, std::string step_pattern);
RcppExport SEXP _IncDTW_IGCM_cpp(SEXP gcmNSEXP, SEXP dmNSEXP, SEXP cmNSEXP, SEXP step_patternSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type gcmN(gcmNSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type dmN(dmNSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type cmN(cmNSEXP);
    Rcpp::traits::input_parameter< std::string >::type step_pattern(step_patternSEXP);
    rcpp_result_gen = Rcpp::wrap(IGCM_cpp(gcmN, dmN, cmN, step_pattern));
    return rcpp_result_gen;
END_RCPP
}
// BACKTRACK_cpp
List BACKTRACK_cpp(Rcpp::IntegerMatrix dm);
RcppExport SEXP _IncDTW_BACKTRACK_cpp(SEXP dmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type dm(dmSEXP);
    rcpp_result_gen = Rcpp::wrap(BACKTRACK_cpp(dm));
    return rcpp_result_gen;
END_RCPP
}
// BACKTRACK2IN_cpp
List BACKTRACK2IN_cpp(Rcpp::IntegerMatrix dm, Rcpp::NumericMatrix diffM);
RcppExport SEXP _IncDTW_BACKTRACK2IN_cpp(SEXP dmSEXP, SEXP diffMSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type dm(dmSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type diffM(diffMSEXP);
    rcpp_result_gen = Rcpp::wrap(BACKTRACK2IN_cpp(dm, diffM));
    return rcpp_result_gen;
END_RCPP
}
// BACKTRACK2II_cpp
List BACKTRACK2II_cpp(Rcpp::IntegerMatrix dm, Rcpp::IntegerMatrix diffM);
RcppExport SEXP _IncDTW_BACKTRACK2II_cpp(SEXP dmSEXP, SEXP diffMSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type dm(dmSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type diffM(diffMSEXP);
    rcpp_result_gen = Rcpp::wrap(BACKTRACK2II_cpp(dm, diffM));
    return rcpp_result_gen;
END_RCPP
}
// normmat
Rcpp::NumericMatrix normmat(Rcpp::NumericMatrix x);
RcppExport SEXP _IncDTW_normmat(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(normmat(x));
    return rcpp_result_gen;
END_RCPP
}
