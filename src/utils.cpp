#include "gsim.h"


std::string sexp_type(SEXP x) {
  switch (TYPEOF(x)) {
  case NILSXP:     return "NILSXP";
  case SYMSXP:     return "SYMSXP";
  case LISTSXP:    return "LISTSXP";
  case CLOSXP:     return "CLOSXP";
  case ENVSXP:     return "ENVSXP";
  case PROMSXP:    return "PROMSXP";
  case LANGSXP:    return "LANGSXP";
  case SPECIALSXP: return "SPECIALSXP";
  case BUILTINSXP: return "BUILTINSXP";
  case CHARSXP:    return "CHARSXP";
  case LGLSXP:     return "LGLSXP";
  case INTSXP:     return "INTSXP";
  case REALSXP:    return "REALSXP";
  case CPLXSXP:    return "CPLXSXP";
  case STRSXP:     return "STRSXP";
  case DOTSXP:     return "DOTSXP";
  case ANYSXP:     return "ANYSXP";
  case VECSXP:     return "VECSXP";
  case EXPRSXP:    return "EXPRSXP";
  case BCODESXP:   return "BCODESXP";
  case EXTPTRSXP:  return "EXTPTRSXP";
  case WEAKREFSXP: return "WEAKREFSXP";
  case S4SXP:      return "S4SXP";
  case RAWSXP:     return "RAWSXP";
  default:         return "<unknown>";
  }
}
