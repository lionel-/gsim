

## curve (invlogit (cbind (1, x/100, .5) %*% coef(fit.3)), add=TRUE)


vseq <- function(gs = NULL, seq = NULL, name = NULL, n = 101) {
  if (is.null(gs) && is.null(seq))
    stop("Need either a gs or a sequence")
  stopifnot(is.null(gs) || is.gsvar(gs))

  if (is.null(name))
    name <- name(gs)
  if (is.null(seq))
    seq <- seq(range(gs)[1], range(gs)[2], length.out = n)

  res <- data.frame(
    seq = seq,
    value = vapply(seq, gs, gs(1, "gsvar"), class = "gsvar", name = name)
  )
  names(res)[1] <- runif(1)

  name(res) <- name
  attr(res, "seq_id") <- names(res)[1]
  attr(res, "seq") <- list(seq)
  class(res) <- c("gs", "seq_gs", "data.frame")

  res
}

x <- vseq


seq_gs <- function(res, seq_id, seq) {
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

  
  attr(res, "seq") <- seq
  attr(res, "seq_id") <- seq_id
  class(res) <- c("gs", "seq_gs")
  res
}

is.seq_gs <- function(x) inherits(x, "seq_gs")


seq_operate_binary <- function(a, b, fun) {
  args <- list(a, b)

  ## ## Need to pass list_gs as whole objects to Map(), and not element
  ## ## by element
  ## ## What happens when there is a seq in the list_gs???
  ## args <- lapply(args, function(item) {
  ##   if (is.list_gs(item))
  ##     list(item)
  ##   else
  ##     item
  ## })

  compound_seqs <- function(a, b) {
    browser(expr = getOption("debug_on"))
    id_a <- seq_id(a)
    id_b <- seq_id(b)

    ## a b

    in_a <- id_b %in% id_a
    twins <- id_b[in_a]

    if (!is.seq_gs(a)) {
      ids <- seq_id(b)
      res <- lapply(b, function(b_) fun(a, b_))
    }

    else if (!is.seq_gs(b)) {
      ids <- seq_id(a)
      res <- lapply(a, function(a_) fun(a_, b))
    }

    ## else if (all(in_a)) {
    else {
      pos <- match(id_b, id_a)
      not_in_b <- ids(b)[!ids(a) %in% ids(b)]
      not_in_a <- ids(a)[!ids(b) %in% ids(a)]
      
      a b

      lapply(not_in_b, function(x) {
        
      })

      ## yop <-
      ## a   b   c   value
      ## 1   1   1    I(gs)
      ## 1   1   2    I(gs)
      ## 1   1   3    I(gs)
      ## 1   2   1    I(gs)

      ## yop3 <-
      ## c   b   d   value
      ## 1   1   1   I(gs)
      ## 1   1   2   I(gs)
      ## 1   1   3   I(gs)
      ## 1   2   1   I(gs)

      ## Repeat yop3 over each a, then merge data.frames
      ## yop + yop3 <-
      ## a   b   c   d   value1  value2  res
      ## 1   1   1   1   
      ## 1   1   1   2   
      ## 1   1   1   3   
      ## 1   1   2   1   
    }


    seqs <- lapply(ids, function(id) {
      if (id %in% seq_id(a))
        attr(a, "seq")[[match(id, seq_id(a))]]
      else
        attr(b, "seq")[[match(id, seq_id(b))]]
    })

    seq_gs(res, ids, seqs)
  }

  Reduce(compound_seqs, args)
}


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


seq_operate_variadic <- function(args, pos, fun) {
  ## Only for gsvar atm

  seq_gs_ids <- vapply(args[pos], seq_id, numeric(1))

  if (!all(seq_gs_ids[1] == seq_gs_ids))
    stop("Can not handle different sequences")

  res <- do.call("Map", c(f = fun, args))
  seq_gs(res, Find(is.seq_gs, args))
}


## Multiple sequences:
## the following will increase list depth
## res <- lapply(args[[seq_gs_pos]], function(arg) {
##   do.call(fun_name, assemble_list(args, seq_gs_pos, list(arg)))
## })



as.function.seq_gs <- function(gs) {
  if (is.gsvar(first(gs)))
    as.function.gsvar(gs)

  else if (is.gsresult(first(gs)))
    as.function.gsresult(gs)

  else stop()
}


as.function.gsvar <- function(gs) {
  function(x) {
     process_seq_in(gs, x) %>%
       process_seq_out
  }
}

as.function.gsresult <- function(gs) {
  function(x, out = "random_sim") {
    res <- process_seq_in(gs, x)

    if (out == "random_sim")
      res <- res[, sample(seq_len(ncol(res)), 1)]
    else
      stop()
    
    process_seq_out(res)
  }
}

process_seq_in <- function(gs, x) {
  ## browser(expr = getOption("debug_on"))
  seq <- attr(gs, "seq")

  if (x < min(seq) || x > max(seq))
    stop(paste0("No extrapolation allowed outside (", min(seq), ", ", max(seq), "). Increase range in vseq call."))

  pos <- match_nearest(x, seq)
  need_interpolate <- !is.null(nth(pos, 2))

  if (need_interpolate)
    (nth(gs, pos[[1]]) + nth(gs, pos[[2]])) / 2
  else
    nth(gs, match(x, seq))
}

process_seq_out <- function(res) {
  if (length(res) > 1)
    as.data.frame(res)
  else
    strip_attributes(res)
}


match_nearest <- function(x, seq) {
  pos <- which(abs(seq - x) == min(abs(seq - x)))

  pos2 <-
    if (x - seq[pos] > 0)
      pos + 1
    else if (x - seq[pos] < 0)
      pos - 1
    else  if (x == seq[pos])
      NULL

  list(pos, pos2)
}


strip_attributes <- function(x) {
  ## Keep dims otherwise matrices will be coerced to vectors etc
  dims <- dim(x)
  attributes(x) <- NULL
  dim(x) <- dims
  x
}


seq_id <- function(x) attr(x, "seq_id")

ids <- function(x) names(x)[-match("value", names(x))]

