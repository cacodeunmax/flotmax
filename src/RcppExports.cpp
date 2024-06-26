// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// edmondsKarp
List edmondsKarp(NumericMatrix capacity, int source, int sink);
RcppExport SEXP _flotmax_edmondsKarp(SEXP capacitySEXP, SEXP sourceSEXP, SEXP sinkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type capacity(capacitySEXP);
    Rcpp::traits::input_parameter< int >::type source(sourceSEXP);
    Rcpp::traits::input_parameter< int >::type sink(sinkSEXP);
    rcpp_result_gen = Rcpp::wrap(edmondsKarp(capacity, source, sink));
    return rcpp_result_gen;
END_RCPP
}
// ford_fulkerson150
List ford_fulkerson150(NumericMatrix graph, int source, int sink);
RcppExport SEXP _flotmax_ford_fulkerson150(SEXP graphSEXP, SEXP sourceSEXP, SEXP sinkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< int >::type source(sourceSEXP);
    Rcpp::traits::input_parameter< int >::type sink(sinkSEXP);
    rcpp_result_gen = Rcpp::wrap(ford_fulkerson150(graph, source, sink));
    return rcpp_result_gen;
END_RCPP
}
// edmondsKarp_gif
List edmondsKarp_gif(NumericMatrix capacity, int source, int sink);
RcppExport SEXP _flotmax_edmondsKarp_gif(SEXP capacitySEXP, SEXP sourceSEXP, SEXP sinkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type capacity(capacitySEXP);
    Rcpp::traits::input_parameter< int >::type source(sourceSEXP);
    Rcpp::traits::input_parameter< int >::type sink(sinkSEXP);
    rcpp_result_gen = Rcpp::wrap(edmondsKarp_gif(capacity, source, sink));
    return rcpp_result_gen;
END_RCPP
}
// ford_fulkerson_gif
List ford_fulkerson_gif(NumericMatrix graph, int source, int sink);
RcppExport SEXP _flotmax_ford_fulkerson_gif(SEXP graphSEXP, SEXP sourceSEXP, SEXP sinkSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< int >::type source(sourceSEXP);
    Rcpp::traits::input_parameter< int >::type sink(sinkSEXP);
    rcpp_result_gen = Rcpp::wrap(ford_fulkerson_gif(graph, source, sink));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_flotmax_edmondsKarp", (DL_FUNC) &_flotmax_edmondsKarp, 3},
    {"_flotmax_ford_fulkerson150", (DL_FUNC) &_flotmax_ford_fulkerson150, 3},
    {"_flotmax_edmondsKarp_gif", (DL_FUNC) &_flotmax_edmondsKarp_gif, 3},
    {"_flotmax_ford_fulkerson_gif", (DL_FUNC) &_flotmax_ford_fulkerson_gif, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_flotmax(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
