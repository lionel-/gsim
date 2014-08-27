
## ## FOR POSTERITY

## operate_on_gs_dplyr <- function(a, b, fun) {
##   class_a <- gsim_class(a)
##   class_b <- gsim_class(b)

##   class(a) <- "data.frame"
##   class(b) <- "data.frame"

##   if (all(c(class_a, class_b) == "posterior")) {
##     c(a, b[-1]) %>%
##       set_names("sim", "object.x", "object.y") %>%
##       do_by_sims_dplyr(gs(fun(.$object.x, .$object.y)))
##   }

##   else if (class_a == "posterior")
##     do_by_sims_dplyr(a, gs(fun(.$object, b)))

##   else if (class_b == "posterior")
##     do_by_sims_dplyr(b, gs(fun(a, .$object)))
  
##   else stop(paste("Class of a:", class_a, "\nClass of b:", class_b))
## }


## posterior_to_numeric <- function(x) {
##   ## might be a bug for simplified vectors
##   dim1 <- dim(first(.subset2(x, 2)))
##   dim2 <- nrow(x) %||% length(.subset2(x, 1))

##   x$object %>%
##     Reduce(f = c, .) %>%
##     set_dim(c(dim1, dim2))
## }


## do_by_sims_vectorized_unary <- function(x, fun, ...) {
##   posterior_to_numeric(x) %>%
##     fun(...) %>%
##     gs("posterior")
## }



## p <- Progress(nrow(gs), min_time = 2)

## %*% not equivalent to +, -, * and /?
## ^ also different? Only works with scalar

## ^ : scalars
## + - * / : scalars, conformable vectors and arrays
##    -> simplify scalars and vectors
## %*% : only conformable arrays
##    -> no simplification


## list_to_posterior <- function(x) {
##   if ((!names(x)[1] == "object") %||% TRUE)
##     x <- list(object = x)
##   c(list(sim = seq_len(length(x[[1]]))), x) %>%
##     quickdf %>%
##     set_gsim_attributes("posterior")
## }

## do_by_sims_dplyr <- function(gs, ...) {
##   if (!is.data.frame(gs))
##     gs <- quickdf(gs)
##   gs <- rowwise(gs)

##   dots <- dots_q(...)
##   if (length(dots) > 1)
##     stop("Only one operation allowed at the moment")
##   else
##     dots <- first(dots)
##   quoted_do <- bquote(do(.(gs), object = .(dots)))

##   done <- eval(quoted_do, envir = parent.frame(), enclos = parent.frame())
##   list_to_posterior(done)
## }

## do_by_sims_vectorized <- function(a, b, fun) {
##   ## todo: check for different input shapes. Maybe automatically
##   ## resort to dplyr or loop for non conformable inputs

##   ## if one of them is posterior, dim = c(100, 3, 4, 5)
##   ## ok if other - is scalar
##   ##             - is dim (3, 4, 5)
##   ##             - is dim (3, 4)?

##   ## Actually, if other shares some of the dims, its length will be a
##   ## multiple and all is ok. The user needs to check that it makes
##   ## sense.

##   ## hmm... better if sims are last dim?? yes................
##   ## is that the case right now? looks like it. Todo: check that
##   ## browser(expr = getOption("debug_on"))

##   if (is.posterior(a))
##     a <- posterior_to_numeric(a)
##   if (is.posterior(b))
##     b <- posterior_to_numeric(b)

##   res <- fun(a, b)
##   dim_len <- dim_length(res)

##   res %>% 
##     aperm(c(dim_len, seq_len(dim_len - 1))) %>%
##     gs("posterior")
## }




## DATA.FRAME POSTERIOR ##

  ## if (class == "posterior") {
  ##   nsims <- nrow(object) %||% length(object)
  ##   res <- list(sim = as.list(seq_len(nsims)))

  ##   ## fixme: breaks as.gsarray
  ##   if (is.null(dim(object)))
  ##     object <- array(object, c(length(object), 1))

  ##   res[["object"]] <- object %>%
  ##     apply(1, list) %>%
  ##     lapply(function(x) {
  ##       colnames <- last(dimnames(x), default = NULL) %||% colnames
  ##       x[[1]] %>%
  ##         as.gsarray %>%
  ##         ## gs("data", colnames = colnames) %>%
  ##         set_colnames(colnames) %>%
  ##         rbind_cols
  ##     })

  ##   attr(res, "row.names") <- .set_row_names(nsims)
  ##   res %>%
  ##     set_gsim_attributes("posterior", group)
  ## }



## variadic_operator <- function(fun, ...) {
##   function(...) {
##     dots <- dots(...)

##     if (length(dots) == 0)
##       NULL
##     else if (length(dots) == 1)
##       first(dots)
##     else
##       Reduce(function(a, b) operate(a, b, fun), dots[-1], first(dots))
##   }
## }


## `%*%.gs` <- function(x, y) {

##   ## HANDLING SEQUENCES
##   if (is.seq_gs(x) || is.seq_gs(y))
##     return(seq_operate(x, y, fun = `%*%.gs`))
  
  
##   ## Transform list of coefficients into proper matrices
##   if (is.list_gs(x)) x <- as.matrix(x)
##   if (is.list_gs(y)) y <- as.matrix(y)
  

##   ## todo: handle all 2^3 combinations
##   if (is.posterior(x) || is.posterior(y)) {
##     nobs <- Find(is.data, list(x, y)) %>% nrow
##     nsims <- get_nsims()
##     res <- array(NA, c(nobs, nsims))

##     if (is.posterior(x)) {
##       for (i in seq(nsims)) {
##         res[, i] <- base::`%*%`(x[i, ], y)
##       }
##     }
##     else if (is.posterior(y)) {
##       for (i in seq(nsims)) {
##         res[, i] <- base::`%*%`(x, y[i, ])
##       }
##     }

##     res <- gs(res, "gsresult")

##   }

##   else {
##     res <- base::`%*%`(cbind(x), cbind(y)) %>%
##       t %>% gs("data")
##   }

##   res
## }



## GROUPED ##
  ## is_a_grouped <- inherits(a, "grouped_gs")
  ## is_b_grouped <- inherits(b, "grouped_gs")

  ## a_group <- group(a)
  ## b_group <- group(b)

  ## if (length(a_group) > 1 || length(b_group) > 1) {
  ##   stop("Todo: One of the objects belongs to more than one group")
  ## } else if (!any(c(is.null(a_group), is.null(b_group))) &&
  ##            a_group != b_group) {
  ##   stop("Objects belong to different groups")
  ## }
  ## ## Objects loose their grouped character when combined with
  ## ## ungrouped objects
  ## else if (is_a_grouped != is_b_grouped)
  ##   operate_on_gs(a, b, fun, post_fun)

  ## else if (is_a_grouped && is_b_grouped)
  ##   operate_on_grouped_gs(a, b, fun)
  ## else
  ##   operate_on_gs(a, b, fun, post_fun)


## expand_grouped_gs <- function(gs) {
##   assert_that(!is.null(group(gs)))

##   if (is.data(gs))
##     gs[get(group(gs))]
##   else
##     gs[get(group(gs)), ]
## }


## operate_on_grouped_gs <- function(a, b, fun) {
##   classes <- c(gsim_class(a), gsim_class(b))

##   if (classes[1] == classes[2]) {
##     result <- operate_on_identically_grouped(a, b, fun)
##   } else if (all(classes %in% c("data", "posterior"))) {
##     result <- operate_on_grouped_param(a, b, fun)
##   } else if (all(classes %in% c("data", "gsresult"))) {
##     result <- operate_on_grouped_var_result(a, b, fun)
##   } else if (all(classes %in% c("posterior", "gsresult"))) {
##     result <- operate_on_grouped_param(a, b, fun)
##   } else stop("Unknown objects")

##   result
## }

## operate_on_identically_grouped <- function(a, b, fun) {
##   class <- gsim_class(a)
##   result <- fun(a, b)

##   gs(result, class, group = group(a))
## }

## operate_on_grouped_param <- function(a, b, fun) {

##   if (!is.grouped_gs(a) || !is.grouped_gs(b)) {
##     if (gsim_class(a) == "data") {
##       var   <- a
##       param <- expand_grouped_gs(b)
##     } else {
##       var   <- b
##       param <- expand_grouped_gs(a)
##     }
##     result <- fun(param, var)
##   } else if (is.grouped_gs(a) && is.grouped_gs(b)) {
##     if (is_any.gsresult(a, b)) {
##       ## Problem: Recognize between varying parameters, with dimension
##       ## (ngroup, niter), and constant parameters, with dimension niter.

##       if (is.gsresult(a) && is.null(dim(b))) {
##         ## assert_that(length(b) == b$properties$niter)
##         param  <- b
##         result <- a
##         result <- sweep(result, 2, param, fun)
##       } else if (is.gsresult(b) && is.null(dim(a))) {
##         ## assert_that(length(a) == a$properties$niter)
##         param  <- a
##         result <- b
##         result <- sweep(result, 2, param, fun)
##       } else if (is.gsresult(b) && length(dim(a)) == 2) {
##         ## assert_that(dim(a) == c(ngroups, niter))
##         param  <- a
##         result <- b
##         result <- fun(param, result)
##       }

##     } else if (gsim_class(a) == "data") {
##       var   <- a
##       param <- b
##       result <- outer(var, param, fun)
##     } else {
##       var   <- b
##       param <- a
##       result <- outer(var, param, fun)
##     }
##   } else {
##     warning("Verify this")
##     result <- fun(a, b)
##   }

##   gs(result, "gsresult", group = group(a))
## }

## operate_on_grouped_var_result <- function(a, b, fun) {
##   result <- fun(a, b)
##   gs(result, "gsresult", group = group(a))
## }


## OLD

  ## params <- slotNames(sims)
  ## mclist <- lapply(params, function(param) slot(sims, param)) %>%
  ##   set_names(params) %>%
  ##   Filter(f = function(x) !(is.null(first(x))))

  ## fill_dims <- function(x) {
  ##   dims <- dim(x)
  ##   if (is.null(dims)) {
  ##     dims <- c(1, length(x), 1)
  ##   } else {
  ##     n <- length(dims)
  ##     if (n < 3)
  ##       dims <- c(dims, rep(1, length.out = 3 - n))
  ##   }
  ##   dims
  ## }

  ## make_mcarray <- function(mcarray, names) {
  ##   names(dim(mcarray)) <- c("", "iteration", "chain")
  ##   dimnames(mcarray) <- list(names, NULL, NULL)
  ##   class(mcarray) <- "mcarray" 
  ##   mcarray
  ## }

  ## matrices_idx <- vapply(mclist, is.matrix, logical(1))
  ## mclist[matrices_idx] %<>% lapply(t)

  ## dims <- lapply(mclist, fill_dims)
  ## dim_names <- lapply(mclist, function(l) dimnames(l)[[1]])

  ## mclist %<>% Map(f = array, ., dims) %>%
  ##   Map(f = make_mcarray, ., dim_names)

  ## class(mclist) <- c("list", "mclist")
  ## mclist



  ## if (class_a == class_b) {
  ##   if (is.grouped_gs(a) && is.posterior(a)) {
  ##     a <- expand_grouped_gs(a)
  ##   } else if (is.grouped_gs(b) && is.posterior(b)) {
  ##     b <- expand_grouped_gs(b)
  ##   }
  ##   result <- fun(a, b) %>%
  ##     gs(class_a)
  ##   return(result)
  ## }



  ## ## Make gsim operations compatible with numeric objects
  ## ## Maybe not necessary since attributes rework
  ## if (is.numeric(a)) {
  ##   a <- gs(a, "data")
  ## } else if (is.numeric(b)) {
  ##   b <- gs(b, "data")
  ## }



  ## ## todo: should not be necessary, even harmful
  ## flatten <- function(list) 
  ##   if (is.list(list) && length(list) == 1)
  ##     list <- list[[1]]
  ##   list
  ## }
  ## a <- flatten(a)
  ## b <- flatten(b)

  ## ## Maybe work this out through should_expand to prevent expanding
  ## ## when the grouped object is operated with a scalar
  ## if (is_any.scalar(a, b)) {
  ##   class <- gsim_class(Find(Negate(is.scalar), list(a, b)))
  ##   result <- fun(a, b) %>%
  ##     gs(class)
  ##   return(result)
  ## }


  ## should_expand <- function(x) is.grouped_gs(x) && (is.data(x) || is.gsresult(x))
  ## if (should_expand(a))
  ##   a <- expand_grouped_gs(a)
  ##  else if (should_expand(b))
  ##    b <- expand_grouped_gs(b)


    ## is_result <- !is.null(dim(first(subset$value)))

    ## ## Interpolate all simulations of gsresults
    ## if (is_result) {

    ##   approx_sims <- function(gs) {
    ##     apply(gs, )
    ##   }
      
    ##   lapply(subset$value, 2, )

    ## }

    ## else {
    ##   xin <- subset[[interp_input]]
    ##   value <- vapply(subset$value, identity, numeric(1))
    ##   out <- approx(xin, value, xout = get(interp_input))
    ##   out$y
    ## }


## match_nearest <- function(x, seq) {
##   pos <- which(abs(seq - x) == min(abs(seq - x)))
##   browser(expr = getOption("debug_on"))

##   pos2 <-
##     if (x - seq[pos] > 0)
##       pos + 1
##     else if (x - seq[pos] < 0)
##       pos - 1
##     else  if (x == seq[pos])
##       NULL

##   list(pos, pos2)
## }


    ## pos <- match_nearest(x, seq)
    ## need_interpolate <- !is.null(nth(pos, 2))

    ## if (need_interpolate)
    ##   (nth(obj$value, pos[[1]]) + nth(obj$value, pos[[2]])) / 2
    ## else
    ##   nth(obj$value, match(x, seq))




## Multiple sequences:
## the following will increase list depth
## res <- lapply(args[[seq_gs_pos]], function(arg) {
##   do.call(fun_name, assemble_list(args, seq_gs_pos, list(arg)))
## })


## seq_operate_binary <- function(a, b, fun) {
##   if (is.seq_gs(a) && is.seq_gs(b))

##     if (seq_id(a) == seq_id(b))
##       res <- Map(fun, a, b)
##     else {
##       res <- list()
##       for (i in seq_along(b)) {
##         res <- c(res, lapply(a, fun, b[[i]]))
##       }
##     }

##   ## Need to differentiate between a and b (instead of supplying them
##   ## both in Map()) in order to pass list_gs as whole objects to
##   ## `fun` instead of element by element.
##   else if (is.seq_gs(a))
##     res <- lapply(a, function(a_) fun(a_, b))

##   else if (is.seq_gs(b))
##     res <- lapply(b, function(b_) fun(a, b_))

##   ## seq_gs(res, Find(is.seq_gs, list(a, b)))
##   seq_gs(res, Filter(is.seq_gs, list(a, b)))
## }



  ## If sequence object is created from several sequences,
  ## interaction. Unless it is the same id.

  ## What should be done when a sequence "a" is operated on a compound
  ## sequence "a:b"?

  ## eg, (x + y) * x
  ## (x + y) yields a sequence of #x * #y elements, with id c(id_x, id_y)
  ## operating with x should then be applied, repeating over each y. Id
  ## should not change.

  ## And x * (x^2 + y)
  ## We have id_x and c(id_x, id_y)
  ## With #x elements in a, and #x * #y in b
  ## 1 * (1 + 11), 2 * (4 + 11), ..., 10 * (100 + 11), 1 * (1 + 12), ...
  ## -> loop over x first, y second

  ## What about (x + y) + (x + y + z)
  ## We have c(id_x, id_y) and c(id_x, id_y, id_z)

  ## More difficult: (x + y) * (y + x)?
  ## c(id_x, id_y) and c(id_y, id_x)
  ## Retain a's order, and loop over b carefully?

  ## ids4: (x + y + z) * (z + x)
  ## c(id_x, id_y, id_z) and c(id_z, id_x)

  ## ids5: (z + x) * (x + y + z)
  ## c(id_z, id_x) and c(id_x, id_y, id_z)
  ## -> c(id_z, id_x, id_y)
  
  ## ids <- list(c(runif(1), runif(1)))
  ## ids2 <- c(ids, list(ids[[1]][1]))
  ## ids3 <- c(ids, list(c(ids[[1]][2], ids[[1]][1])))
  ## ids4 <- c(list(c(ids[[1]][1], runif(1), ids[[1]][2])), list(c(ids[[1]][2], ids[[1]][1])))
  ## ids5 <- c(ids4[2], ids4[1])
  
  ## browser(expr = getOption("debug_on"))
  ## ids <- lapply(seq_objs, function(obj) attr(obj, "seq_id"))

  ## compound_ids <- function(a, b) {
  ##   in_a <- b %in% a
  ##   twins <- b[in_a]

  ##   if (all(in_a))
  ##     a

  ##   else if (length(in_a) == 0)
  ##     c(a, b)

  ##   else if (length(b) > length(a)) {
  ##     positions <- match(b, a)
  ##     sorted_b <- b[order(positions, na.last = NA)]
  ##     c(sorted_b, b[!in_a])
  ##   }
      
  ##   else
  ##     c(a, b[!in_a])
  ## }
  ## new_ids <- Reduce(compound_ids, ids)

  
  ## attr(res, "seq") <- seq
  ## attr(res, "seq_id") <- seq_id




  ## ## Need to pass list_gs as whole objects to Map(), and not element
  ## ## by element
  ## ## What happens when there is a seq in the list_gs???
  ## args <- lapply(args, function(item) {
  ##   if (is.list_gs(item))
  ##     list(item)
  ##   else
  ##     item
  ## })


    ## seqs <- lapply(ids, function(id) {
    ##   if (id %in% seq_id(a))
    ##     attr(a, "seq")[[match(id, seq_id(a))]]
    ##   else
    ##     attr(b, "seq")[[match(id, seq_id(b))]]
    ## })



      #   yop <-
      #     a   b   c   value
      #     1   1   1    I(gs)
      #     1   1   2    I(gs)
      #     1   1   3    I(gs)
      #     1   2   1    I(gs)

      #   yop3 <-
      #     c   b   d   value
      #     1   1   1   I(gs)
      #     1   1   2   I(gs)
      #     1   1   3   I(gs)
      #     1   2   1   I(gs)

      #   Repeat yop3 over each a, then merge data.frames
      #   yop + yop3 <-
      #     a   b   c   d   value1  value2  res
      #     1   1   1   1   
      #     1   1   1   2   
      #     1   1   1   3   
      #     1   1   2   1   

      ## Important: arrange common columns in a and b in the same manner




    ##   pos <- match(id_b, id_a)

    ##   ## probably way easier with dataframes....

    ##   ## yop <-
    ##   ## a   b   c   value
    ##   ## 1   1   1    I(gs)
    ##   ## 1   1   2    I(gs)
    ##   ## 1   1   3    I(gs)
    ##   ## 1   2   1    I(gs)

    ##   ## yop3 <-
    ##   ## c   b   d   value
    ##   ## 1   1   1   I(gs)
    ##   ## 1   1   2   I(gs)
    ##   ## 1   1   3   I(gs)
    ##   ## 1   2   1   I(gs)

    ##   ## Repeat yop3 over each a, then merge data.frames
    ##   ## yop + yop3 <-
    ##   ## a   b   c   d   value1  value2  res
    ##   ## 1   1   1   1   
    ##   ## 1   1   1   2   
    ##   ## 1   1   1   3   
    ##   ## 1   1   2   1   

    ##   ## yop <- 
    ##   ##   list(a1 = list(b1 = list(c1, c2, c3), b2 = list(c1, c2, c3)),
    ##   ##        a2 = list(b1 = list(c1, c2, c3), b2 = list(c1, c2, c3)))

    ##   ## yop2 <-
    ##   ##   list(c1 = list(b1, b2), c2 = list(b1, b2), c3 = list(b1, b2))

    ##   ## yop3 <-
    ##   ##   list(c1 = list(b1 = list(d1, d2, d3), b2 = list(d1, d2, d3)),
    ##   ##        c2 = list(b1 = list(d1, d2, d3), b2 = list(d1, d2, d3)),
    ##   ##        c3 = list(b1 = list(d1, d2, d3), b2 = list(d1, d2, d3)))

    ##   ## yop + yop3 <-
    ##   ##   list(a1 = list(b1 = list(c1 = list(d1, d2, d3),
    ##   ##                      c2 = list(d1, d2, d3),
    ##   ##                      c3 = list(d1, d2, d3)),
    ##   ##            b2 = list(c1 = list(d1, d2, d3),
    ##   ##                c2 = list(d1, d2, d3),
    ##   ##                c3 = list(d1, d2, d3)))
    ##   ##        a2 = list(b1 = list(c1 = list(d1, d2, d3),
    ##   ##                      c2 = list(d1, d2, d3),
    ##   ##                      c3 = list(d1, d2, d3)),
    ##   ##            b2 = list(c1 = list(d1, d2, d3),
    ##   ##                c2 = list(d1, d2, d3),
    ##   ##                c3 = list(d1, d2, d3))))


    ##   ## Apply recursively until one matching seq is found in the
    ##   ## list depth. Then for loop in which we once again apply
    ##   ## recursively. When we get to the bottom of both lists, and
    ##   ## side = "lhs", apply fun to seq_lapply(rhs, side =
    ##   ## "rhs"). If side = "rhs", return values.

    ##   ## What about situations where rhs is deeper? How can we add
    ##   ## the depth to resulting list? Specify it in the call, then
    ##   ## replicate
      
    ##   ## Find 
    ##   lseqapply <- function(seq, rhs = NULL, new_seqs = NULL) {
    ##     browser(expr = getOption("debug_on"))

    ##     if (is.list(seq) && seq_id(seq) == ids)
    ##       lapply(seq, lseqapply, rhs = rhs, new_seqs = new_seqs)

    ##     else {
    ##       if (is.null(new_seqs))
    ##         lapply(rhs, lseqapply)
    ##       else
    ##         fun(seq, rhs)
    ##     }
    ##   }
    ## }

    ## lapply(a, lseqapply, rhs = b)

    ## ## else if (!any(in_a)) {
    ## ##   ids <- c(id_a, id_b)
      
    ## ##   res <- list()
    ## ##   for (i in seq_along(b)) {
    ## ##     res <- c(res, lapply(a, fun, b[[i]]))
    ## ##   }
    ## ## }

    ## ## else if (length(id_b) > length(id_a)) {
    ## ##   positions <- match(id_b, id_a)
    ## ##   sorted_id_b <- id_b[order(positions, na.last = NA)]
    ## ##   ids <- c(sorted_id_b, id_b[!in_a])
    ## ## }
      
    ## ## else if (length(id_b) < length(id_a)) {
    ## ##   browser(expr = getOption("debug_on"))
    ## ##   ids <- c(id_a, id_b[!in_a])
    ## ## }

    ## ## else stop("uh oh")



