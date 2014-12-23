
# gorgeous simulations

gsim is a DSL for statistical simulations. Its goals are:

* Automating loops over simulations, letting you focus on the
  mathematics of your computations rather than the technicalities.

  This should not come at the expense of performance. gsim tries to
  vectorise operations when appropriate.

* Help you creating numerical and graphical summaries of parameters
  and fitted models.

Have a look at the
[introduction vignette](https://dl.dropboxusercontent.com/u/239966/gsim.html)
for a tutorial.

To install the package:

```{r}
devtools::install_github("lionelgit/gsim")
```


# Future plans

Currently, gsim's workhorse is an object that is both a container and
a function. It contains data, simulations of statistical parameters,
and is used as a function to operate on these contents. The results
can then be retrieved from the container in several ways.

```{r}
sims <- gsim(stan_sims, data1, data2)
sims({
  new1 <- param1 * var1
  new2 <- param2 + param1
})

# Retrieve new1 in the form of a tidy data frame
out1 <- sims(new1)

# Retrieve new2 unaltered, in the form of a numeric array
out2 <- sims$new2
```

This workflow may seem unusual to the experimented R programmer and
indeed it is. When you instruct a container to create new variables,
as in the example above, it actually modifies itself to hold them
permanently. You don't have to reassign the container with an updated
version using `<-`. Newly created variables are invisibly created
inside the existing object. In computer science parlance, this
behavior is known as a
[side effect](http://en.wikipedia.org/wiki/Side_effect_(computer_science)):
a gsim container modifies itself as a side effect before returning the
object mentioned in the last line of the instructions.

However, such side effects should generally be avoided in R because it
is a
[functional language](http://adv-r.had.co.nz/Functional-programming.html).
In the functional approach, the only impact functions should have on
their environment is through the output they return. The next version
of gsim will therefore feature a new idiom more in line with R's
conventions. Gsim will operate directly on R objects containing
simulations, and return a new updated object. In turn, this object can
be reassigned to update the original one.

To make the workflow seamless, the old functional idiom will be
complemented with a new one:
[magrittr](https://github.com/smbache/magrittr)'s pipes and
[dplyr](https://github.com/hadley/dplyr)'s verbs. The function
`mutate()` will operate on parameters simulation by simulation,
automating the loops as usual. Then the verb `summarise()` will be
used to apply a function over the simulations, e.g. to obtain the
posterior mean or some quantiles of the new quantities.

The example above will translate to:
```{r}
sims <- extract(stan_fit) %>%
  mutate(new1 = param1 * var1)

ggdata <- sims %>%
  mutate(new2 = new1 + param2) %>%
  summarise(
    new2_mean = mean(new1),
    new2_q = quantiles(new1, c(0.025, 0.975))
  ) %>%
  tidy()
```

In the second part of this example, `new2` is created by `mutate()`,
but it is never assigned to `sims`. It is assigned to `ggdata` after a
visit to `summarise()` and `tidy()`. Any change to the environment
occurs through the `<-` operator. Since there is no side effect, the
object `sims` was not modified to hold `new2`.

`mutate()` is best suited for simple transformations. More complex
operations will be handled by the verb `by_sim()`. It will be similar
to how gsim currently applies operations, but with a functional
touch. In addition, there will be a distinction between creating
temporary variables and new transformations of parameters. The
assignment operator `<-` will only create temporary variables. To
actually modify the list of simulations returned by `by_sims()` with
new variables, they will have to be assigned with `~`. In the
following example, sims is updated with `new2`, but not `new1`.

```{r}
sims <- sims %>%
  by_sim({
    new1 <- param1 * var1
    new2 ~ param2 + param1
  })
```

Finally, `by_sim()` will also be used to turn any function into an
automatic looper. In effect, `by_sims()` will expose the parameters in
the scope of the function, and apply the function to each simulation.

```{r}
my_sd <- by_sim(sd)
stan_fit %>%
  my_sd(new1)
```
