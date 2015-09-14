#include "gsim.h"


/* Could be made more efficient by exploiting tail optimisation */

strings& find_names_recurser(SEXP expr, strings& names) {
  switch(TYPEOF(expr)) {
  case REALSXP:
  case INTSXP:
  case CHARSXP:
  case LGLSXP:
  case CPLXSXP:
  case STRSXP:
  case NILSXP:
    break;

  case LANGSXP: {
    find_names_recurser(CDR(expr), names);
    break;
  }

  case LISTSXP: {
    SEXP shrunkable = expr;
    while (!Rf_isNull(shrunkable)) {
      find_names_recurser(CAR(shrunkable), names);
      shrunkable = CDR(shrunkable);
    }
    break;
  }

  case VECSXP: {
    Rcout << "vecsxp" << std::endl;
    for (int i = 0; i != Rf_length(expr); ++i) {
      find_names_recurser(get_vector_elt(expr, i), names);
    }
    break;
  }

  case SYMSXP: {
    std::string symbol_name = CHAR(PRINTNAME(expr));
    names.push_back(symbol_name);
    break;
  }

  default:
    stop("Internal error: expr not recognized: %s", sexp_type(expr));
  }

  return names;
}


// [[Rcpp::export]]
strings find_names(SEXP call) {
  strings names;
  return find_names_recurser(call, names);
}
