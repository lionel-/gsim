#include "gsim.h"


strings& copy_colnames(strings& col_names, SEXP obj, int i);


/* Slightly faster than as.matrix() or unlist() %>% set_dim()
   Supports lists of vectors and matrices */

// [[Rcpp::export]]
SEXP as_matrix_impl(const SEXP data) {
  if (TYPEOF(data) != VECSXP) {
    stop("as_matrix() only convert lists");
  };
  if (Rf_length(data) == 0) {
    return Rf_allocMatrix(REALSXP, 0, 0);
  }

  int n_elements = Rf_length(data);
  strings data_names = as<strings>(Rf_getAttrib(data, R_NamesSymbol));
  strings col_names;

  std::vector<int> elt_cols(n_elements);
  int n_rows;
  int n_cols = 0;

  SEXP elt = get_vector_elt(data, 0);
  SEXP dims, dims_names;
  if (Rf_isMatrix(elt)) {
    dims = Rf_getAttrib(elt, R_DimSymbol);
    n_rows = INTEGER(dims)[0];
    n_cols += INTEGER(dims)[1];
    copy_colnames(col_names, elt, 0);
  } else {
    n_rows = Rf_length(elt);
    n_cols += 1;
    col_names.push_back(data_names[0]);
  }
  elt_cols[0] = n_cols;

  if (n_elements > 1) {
    for (int i = 1; i != n_elements; ++i) {
      int current_n_rows;
      elt = get_vector_elt(data, i);

      if (TYPEOF(elt) != REALSXP) {
        stop("all variables should be of type real");
      }

      if (Rf_isMatrix(elt)) {
        dims = Rf_getAttrib(elt, R_DimSymbol);
        current_n_rows = INTEGER(dims)[0];
        elt_cols[i] = INTEGER(dims)[1];
        n_cols += elt_cols[i];
        copy_colnames(col_names, elt, i);
      } else if (Rf_isArray(elt)) {
        stop("arrays are not supported");
      } else {
        current_n_rows = Rf_length(elt);
        elt_cols[i] = 1;
        n_cols += 1;
        col_names.push_back(data_names[i]);
      }
      if (current_n_rows != n_rows) {
        stop("all variables should have the same length");
      }
    }
  }

  Shield<SEXP> mat(Rf_allocMatrix(REALSXP, n_rows, n_cols));
  for (int i = 0; i != n_elements; ++i) {
    elt = get_vector_elt(data, i);
    memcpy((char*)REAL(mat) + i * n_rows * sizeof(double),
           (char*)REAL(elt),
           sizeof(double) * n_rows * elt_cols[i]);
  }

  Shield<SEXP> dim_names(Rf_allocVector(VECSXP, 2));
  set_vector_elt(dim_names, 0, R_NilValue);
  set_vector_elt(dim_names, 1, wrap(col_names));
  Rf_setAttrib(mat, R_DimNamesSymbol, dim_names);

  return mat;
}


strings& copy_colnames(strings& col_names, SEXP obj, int i) {
  SEXP dims = Rf_getAttrib(obj, R_DimSymbol);
  SEXP dims_names = Rf_getAttrib(obj, R_DimNamesSymbol);
  SEXP dims_colnames = get_vector_elt(dims_names, 1);
  if (Rf_isNull(dims_names) || Rf_isNull(dims_colnames)) {
    strings new_names(INTEGER(dims)[1], "");
    col_names.insert(col_names.end(), new_names.begin(), new_names.end());
  } else {
    strings new_names = as<strings>(dims_colnames);
    col_names.insert(col_names.end(), new_names.begin(), new_names.end());
  }
  return col_names;
}
