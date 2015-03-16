#include "gsim.h"


strings find_params(strings param_names, strings fun_args) {
  strings params;
  std::sort(fun_args.begin(), fun_args.end());
  std::sort(param_names.begin(), param_names.end());

  std::set_intersection(fun_args.begin(), fun_args.end(),
                        param_names.begin(), param_names.end(),
                        std::back_inserter(params));
  return params;
}


NumericVector& sim_array(NumericVector& array, int major_order = 1) {
  array.attr("class") = CharacterVector::create("sims_array", "array");
  array.attr("major_order") = LogicalVector::create(major_order ? true : false);
  return array;
}


// [[Rcpp::export]]
SEXP by_sim_impl(const List sims, NumericVector data_lengths, int n_sims,
                 const SEXP fun, const Environment calling_env) {
  if (Rf_isNull(sims.names())) {
    stop("simulations must be named");
  }

  // Find subset of parameters actually used in the function
  strings params = find_params(
    as<strings>(sims.names()),
    as<strings>(Rf_getAttrib(FORMALS(fun), R_NamesSymbol))
  );
  CharacterVector params_sexp = wrap(params);
  int n_params = params.size();
  List sims_subset = sims[params_sexp];
  data_lengths = data_lengths[params_sexp];

  // Create function call
  Shield<SEXP> lang_call(Rf_lang1(fun));
  Shield<SEXP> args(Rf_allocList(params.size()));
  SEXP walker = args;

  for (int i = 0; i!= params.size(); ++i) {
    SEXP symbol = Rf_install(params[i].c_str());
    SETCAR(walker, symbol);
    SET_TAG(walker, symbol);
    walker = CDR(walker);
  }
  SETCDR(lang_call, args);


  // Create buffers for the necessary copies of parameters
  List params_buffers = no_init(params.size());

  // Apply function to first simulation to get output dimensions
  Environment eval_env = new_env(calling_env);

  for (int i = 0; i != params.size(); ++i) {
    NumericVector array(sims_subset[i]);
    NumericVector param_buffer = no_init(data_lengths[i]);
    params_buffers[i] = param_buffer;

    for (int j = 0; j != data_lengths[i]; ++j) {
      param_buffer[j] = array[j];
    }
    eval_env.assign(params[i], param_buffer);
  }

  // Allocate output vector based on dimensions of first-sim output
  NumericVector first(Rf_eval(lang_call, eval_env));
  int out_length = first.size();
  NumericVector out = no_init(out_length * n_sims);

  // Copy
  for (int i = 0; i != out_length; ++i) {
    out[i] = first[i];
  }


  // Now, apply function to each simulation
  for (int sim = 1; sim != n_sims; ++sim) {
    for (int param = 0; param != params.size(); ++param) {
      NumericVector array(sims_subset[param]);
      NumericVector param_buffer(params_buffers[param]);

      for (int i = 0; i != data_lengths[param]; ++i) {
        param_buffer[i] = array[i + sim * data_lengths[param]];
      }
      eval_env.assign(params[param], param_buffer);
    }

    NumericVector output_buffer(Rf_eval(lang_call, eval_env));
    for (int i = 0; i != out_length; ++i) {
      out[i + (sim * out_length)] = output_buffer[i];
    }
  }

  // Copy relevant attributes and create sim_array object
  NumericVector out_data_dims;
  if (Rf_isNull(first.attr("dim"))) {
    out_data_dims = first.size();
  } else {
    out_data_dims = first.attr("dim");
  }
  NumericVector out_dims(out_data_dims.size() + 1);
  std::copy(out_data_dims.begin(), out_data_dims.end(), out_dims.begin());
  out_dims[out_dims.size() - 1] = n_sims;
  out.attr("dim") = out_dims;

  out = sim_array(out);

  return out;
}
