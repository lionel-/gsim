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



