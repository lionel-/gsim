#include "gsim.h"


strings find_params(strings param_names, strings fun_args) {
  strings found;
  std::sort(fun_args.begin(), fun_args.end());
  std::sort(param_names.begin(), param_names.end());

  std::set_intersection(fun_args.begin(), fun_args.end(),
                        param_names.begin(), param_names.end(),
                        std::back_inserter(found));
  return found;
}


NumericVector& sims_array(NumericVector& array, int sims_major = 1) {
  int ndims = Rf_length(array.attr("dim"));
  List dimnames(ndims);
  CharacterVector dimnames_names(ndims);
  int sims_i = sims_major ? ndims - 1 : 1;
  dimnames_names[sims_i] = "iterations";
  dimnames.names() = dimnames_names;
  array.attr("dimnames") = dimnames;

  array.attr("class") = CharacterVector::create("sims_array", "array");
  array.attr("sims_major") = LogicalVector::create(sims_major ? true : false);

  return array;
}


// [[Rcpp::export]]
SEXP by_sim_impl(const List sims, List data_dims, int n_sims,
                 const SEXP fun, const Environment eval_env, SEXP dots) {
  if (Rf_isNull(sims.names())) {
    stop("Simulations must be named");
  }
  strings orig_names_strings = sims.names();

  // Find subset of parameters actually used in the function
  strings params_strings = find_params(
    orig_names_strings, as<strings>(Rf_getAttrib(FORMALS(fun), R_NamesSymbol))
  );
  CharacterVector params = wrap(params_strings);

  int n_params = params_strings.size();
  List sims_subset = sims[params];
  List dims_subset = data_dims[params];

  NumericVector data_lengths = no_init(dims_subset.size());
  for (int i = 0; i < n_params; ++i) {
    NumericVector dims = dims_subset[i];
    data_lengths[i] = std::accumulate(dims.begin(), dims.end(),
                                      1, std::multiplies<int>());
  }
  data_lengths.names() = params;


  // Create function call
  Shield<SEXP> lang_call(Rf_lang1(fun));
  Shield<SEXP> args(Rf_allocList(n_params + Rf_length(dots)));
  SEXP walker = args;

  for (int i = 0; i < n_params; ++i) {
    SEXP symbol = Rf_install(params_strings[i].c_str());
    SETCAR(walker, symbol);
    SET_TAG(walker, symbol);
    walker = CDR(walker);
  }

  if (Rf_length(dots) > 0) {
    SET_TYPEOF(dots, LANGSXP);
    dots = CDR(dots);  // car is the list that was used to capture dots
    SETCAR(walker, dots);
  }

  SETCDR(lang_call, args);


  // Create buffers for the unavoidable copies of parameters
  List params_buffers = no_init(n_params);

  // Apply function to first simulation to get output dimensions
  for (int i = 0; i < n_params; ++i) {
    int length = data_lengths[i];
    length = length == 0 ? 1 : length;

    NumericVector array(sims_subset[i]);
    NumericVector param_buffer = no_init(length);
    params_buffers[i] = param_buffer;

    for (int j = 0; j < length; ++j) {
      param_buffer[j] = array[j];
    }

    // Set data dimensions
    NumericVector param_dims = dims_subset[i];
    if (param_dims.size() > 0) {
      param_buffer.attr("dim") = param_dims;
    }

    // Store first simulation of param i in the evaluation env
    eval_env.assign(params_strings[i], param_buffer);
  }

  // Allocate output vector based on dimensions of first-sim output
  NumericVector first(Rf_eval(lang_call, eval_env));
  int out_length = first.size();
  NumericVector out = no_init(out_length * n_sims);

  // Copy result of first simulation to output vector
  for (int i = 0; i != out_length; ++i) {
    out[i] = first[i];
  }

  // Now, apply function to each simulation
  // Todo: make this dirty repeat a bit dryer
  for (int sim = 1; sim < n_sims; ++sim) {
    for (int param_i = 0; param_i < n_params; ++param_i) {
      int param_length = data_lengths[param_i];
      param_length = param_length == 0 ? 1 : param_length;

      NumericVector array(sims_subset[param_i]);
      NumericVector param_buffer(params_buffers[param_i]);

      for (int i = 0; i < param_length; ++i) {
        param_buffer[i] = array[i + sim * param_length];
      }

      // Set data dimensions 
      NumericVector param_dims = dims_subset[param_i];
      if (param_dims.size() > 0) {
        param_buffer.attr("dim") = param_dims;
      }

      eval_env.assign(params_strings[param_i], param_buffer);
    }

    NumericVector output_buffer(Rf_eval(lang_call, eval_env));
    for (int i = 0; i < out_length; ++i) {
      out[i + (sim * out_length)] = output_buffer[i];
    }
  }

  // Copy relevant attributes and create sim_array object
  // Scalar parameters should not have dim attribute
  if (!Rf_isNull(first.attr("dim"))) {
    NumericVector out_data_dims = first.attr("dim");
    NumericVector out_dims(out_data_dims.size() + 1);
    std::copy(out_data_dims.begin(), out_data_dims.end(), out_dims.begin());
    out_dims[out_dims.size() - 1] = n_sims;
    out.attr("dim") = out_dims;
  } else {
    NumericVector out_dims = wrap(n_sims);
    out.attr("dim") = out_dims;
  }

  return sims_array(out);
}
