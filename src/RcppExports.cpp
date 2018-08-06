// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// dtlz_1
SEXP dtlz_1(arma::vec x, int M);
RcppExport SEXP _smoof_dtlz_1(SEXP xSEXP, SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(dtlz_1(x, M));
    return rcpp_result_gen;
END_RCPP
}
// dtlz_2
SEXP dtlz_2(arma::vec x, int M);
RcppExport SEXP _smoof_dtlz_2(SEXP xSEXP, SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(dtlz_2(x, M));
    return rcpp_result_gen;
END_RCPP
}
// dtlz_3
SEXP dtlz_3(arma::vec x, int M);
RcppExport SEXP _smoof_dtlz_3(SEXP xSEXP, SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(dtlz_3(x, M));
    return rcpp_result_gen;
END_RCPP
}
// dtlz_4
SEXP dtlz_4(arma::vec x, int M, double alpha);
RcppExport SEXP _smoof_dtlz_4(SEXP xSEXP, SEXP MSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(dtlz_4(x, M, alpha));
    return rcpp_result_gen;
END_RCPP
}
// dtlz_5
SEXP dtlz_5(arma::vec x, int M);
RcppExport SEXP _smoof_dtlz_5(SEXP xSEXP, SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(dtlz_5(x, M));
    return rcpp_result_gen;
END_RCPP
}
// dtlz_6
SEXP dtlz_6(arma::vec x, int M);
RcppExport SEXP _smoof_dtlz_6(SEXP xSEXP, SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(dtlz_6(x, M));
    return rcpp_result_gen;
END_RCPP
}
// dtlz_7
SEXP dtlz_7(arma::vec x, int M);
RcppExport SEXP _smoof_dtlz_7(SEXP xSEXP, SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(dtlz_7(x, M));
    return rcpp_result_gen;
END_RCPP
}
// kursawe
NumericVector kursawe(arma::vec x);
RcppExport SEXP _smoof_kursawe(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(kursawe(x));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP evaluateBBOBFunctionCPP(SEXP, SEXP, SEXP, SEXP);
RcppExport SEXP evaluateUFFunction(SEXP, SEXP, SEXP);
RcppExport SEXP getOptimumForBBOBFunctionCPP(SEXP, SEXP, SEXP);
RcppExport SEXP mof_bk1(SEXP);
RcppExport SEXP mof_MOP1(SEXP);
RcppExport SEXP mof_MOP2(SEXP);
RcppExport SEXP mof_MOP3(SEXP);
RcppExport SEXP mof_MOP4(SEXP);
RcppExport SEXP mof_MOP5(SEXP);
RcppExport SEXP mof_MOP6(SEXP);
RcppExport SEXP mof_MOP7(SEXP);
RcppExport SEXP mof_viennet(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_smoof_dtlz_1", (DL_FUNC) &_smoof_dtlz_1, 2},
    {"_smoof_dtlz_2", (DL_FUNC) &_smoof_dtlz_2, 2},
    {"_smoof_dtlz_3", (DL_FUNC) &_smoof_dtlz_3, 2},
    {"_smoof_dtlz_4", (DL_FUNC) &_smoof_dtlz_4, 3},
    {"_smoof_dtlz_5", (DL_FUNC) &_smoof_dtlz_5, 2},
    {"_smoof_dtlz_6", (DL_FUNC) &_smoof_dtlz_6, 2},
    {"_smoof_dtlz_7", (DL_FUNC) &_smoof_dtlz_7, 2},
    {"_smoof_kursawe", (DL_FUNC) &_smoof_kursawe, 1},
    {"evaluateBBOBFunctionCPP",      (DL_FUNC) &evaluateBBOBFunctionCPP,      4},
    {"evaluateUFFunction",           (DL_FUNC) &evaluateUFFunction,           3},
    {"getOptimumForBBOBFunctionCPP", (DL_FUNC) &getOptimumForBBOBFunctionCPP, 3},
    {"mof_bk1",                      (DL_FUNC) &mof_bk1,                      1},
    {"mof_MOP1",                     (DL_FUNC) &mof_MOP1,                     1},
    {"mof_MOP2",                     (DL_FUNC) &mof_MOP2,                     1},
    {"mof_MOP3",                     (DL_FUNC) &mof_MOP3,                     1},
    {"mof_MOP4",                     (DL_FUNC) &mof_MOP4,                     1},
    {"mof_MOP5",                     (DL_FUNC) &mof_MOP5,                     1},
    {"mof_MOP6",                     (DL_FUNC) &mof_MOP6,                     1},
    {"mof_MOP7",                     (DL_FUNC) &mof_MOP7,                     1},
    {"mof_viennet",                  (DL_FUNC) &mof_viennet,                  1},
    {NULL, NULL, 0}
};

RcppExport void R_init_smoof(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
