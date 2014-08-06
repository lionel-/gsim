## Todo: all grouped objects should be lists with two slots: 'group',
## containing the name of the expanded grouping variable, 'data',
## containing the group level data. No expanded data in memory because
## subsetting with grouping variable is very fast. Actually 'group'
## can be in the metadata.

## Then operations with two grouped objects can occur with grouped
## data. But then need to mirror the expanded data as well... Can be
## done on a need-to basis, with a flag. And the expanded data is kept
## in the object only if a global option says so (since this can be
## quite greedy in memory for large datasets with large models). A
## local option, set by gsload or gsattach, would override the global
## option.


## Maybe create an additional slot in gsobjects specifying the
## metadata (length of dataset, number of iterations, ...), so that we
## can check that everything is fine.


## Force grouped operations? Multiply a grouped param with a level 1
## variable which is to be averaged by groups. Could be achieved with
## a function, average_by().


## How to make functions, for example to plot regression lines?
## Function x(obj, n), sampling n rows from the gsresult:
##   - Could return a melted dataframe if given 'n' argument. Retrieve
##     all simulations when n = sim_length (don't sample to preserve order).
##   - Could return a function operating the enclosed gsresult.
##     Problem: how to add objects to the function? Make its first
##     argument the object to add. Keep which rows to remove in a
##     metadata. First argument should evaluate the combination rather
##     than taking a gsobject. Maybe can pass named arguments when
##     adding?
##   - Then would need a new ggplot stat_function, if possible.
##   -> result(x) should return 1 data point sampled from the posterior.
##      Then easy to use with curve. Easy to provide a new stat_function
##      gs_stat_function calling n times stat_function.
##   - Need result(x, stat = "mean") return the posterior mean.
##   - Randomize on creation. Then argument id = . to select one of
##     the iteration. Then can return one value for the same mcmc
##     iteration everytime it's called (needed for curve).
##   - Difficulty: should be able to call exp() or logit() over the
##     whole stuff. Need to write a function operator for everyone of them.
##     The resulting function would check that the x() function it receives has a
##     particular flag in its environment, otherwise throw an error.


## Need subset operator for grouped objects, so that they can be
## explicitely linked to groups with [group]


## Rewrite everything with attributes so it's easier to use with
## non-gsim stuff?


## Modeling needs:
##  - Subset a particular row of a variable or column of a random effect.


## Should keep mean.gs etc hidden in package space, and only load them
## explicitely. Would be accessible from inside local()? or something.



## fit2_gsim <- gsim(ls_subdata, sims_mclist)

## pred <- fit2_gsim(
##   (beta$intercept + education * beta$education +
##     education^2 * beta$education_quad + age * beta$age +
##     female * beta$female) %>% mean
## )

## # so, allow vectorized operations?
## pred2 <- fit2_gsim({
##   X <- cbind(intercept(), education, education^2, age, female)
##   mean(X %*% beta)
## })


NULL
