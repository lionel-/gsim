
# gsim

gsim is a DSL for statistical simulations. It seeks to:

* Provide tools to setup your data for Stan or Jags.

* Automate loops over simulations in an efficient way to let you focus
  on the mathematics of your computations rather than the programming
  technicalities.

* Help you create numerical and graphical summaries of parameters and
  fitted models.

gsim is designed to work with the magrittr pipe `%>%` to create clean
and readable pipelines of objects transformations. To install the
package:

```{r}
install.packages("devtools")
devtools::install_github("lionel-/gsim")
```


## Setup data

* `define()` is like dplyr's `mutate()` method but it works on lists
  and discards any variables that are not mentioned. This last
  property is useful with samplers such as Jags that will complain
  when they find unused variables in the input.

* `design()` creates design matrices. The `.unjoin` argument allows to
  reduce repeated group-level data to one value per group.

```{r}
# Fetch Radon dataset from Stan's repo
radon_data <- radon_data()

# Create the list of data, including design matrices
radon_data <- radon_data %>%
  define(
    y, county,
    X = design(1, x),
    U = design(1, u, .unjoin = county),
    N = nrow(.),
    J = n_distinct(county),
    P_X = ncol(X),
    P_U = ncol(U)
  )

# Get example model code
radon_model <- radon_model(language = "stan", variant = "centered")

# Fit the model
stanfit <- rstan::stan(model_code = radon_model, data = radon_data)
```


## dplyr-like verbs

You can work directly from a Stan or Jags object with gsim
functions. They will be converted to an object of class `sims` which
is simply a list of simulations arrays with some additional
information.

* `select()` parameters from the list. You can select directly by name
  or use one of the matchers from dplyr. Here we select `sigma` and all
  parameters whose name contains `Beta`.

* `mutate()` applies a transformation datawise.

* `summarise()` applies a transformation simulationwise. In other
words, it creates posterior summaries.

Note that unlike the method in dplyr, `summarise()` does not discard
the other variables: `sims` lists can contain both `sims_array` and
normal objects, with the only constraint to keep all the `sims_array`
objects consistent (i.e., same number of simulations).

```{r}
sims <- stanfit %>%
  select(sigma, contains("Beta")) %>%
  mutate(Beta_res = Beta - cols_mean(Beta)) %>%
  summarise(
    post_sd = sd(Beta_res),
    post_mean = mean(Beta_res)
  )

sims
#> 100 simulations
#>
#>       sigma: [1:1]
#>        Beta: [1:85, 1:2]
#>  sigma_Beta: [1:2]
#>  Sigma_Beta: [1:2, 1:2]
#>    Beta_res: [1:85, 1:2]
#>
#> Other objects: post_sd, post_mean
```
