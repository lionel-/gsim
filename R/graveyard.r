
      pos <- match(id_b, id_a)

      ## probably way easier with dataframes....

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

      ## yop <- 
      ##   list(a1 = list(b1 = list(c1, c2, c3), b2 = list(c1, c2, c3)),
      ##        a2 = list(b1 = list(c1, c2, c3), b2 = list(c1, c2, c3)))

      ## yop2 <-
      ##   list(c1 = list(b1, b2), c2 = list(b1, b2), c3 = list(b1, b2))

      ## yop3 <-
      ##   list(c1 = list(b1 = list(d1, d2, d3), b2 = list(d1, d2, d3)),
      ##        c2 = list(b1 = list(d1, d2, d3), b2 = list(d1, d2, d3)),
      ##        c3 = list(b1 = list(d1, d2, d3), b2 = list(d1, d2, d3)))

      ## yop + yop3 <-
      ##   list(a1 = list(b1 = list(c1 = list(d1, d2, d3),
      ##                      c2 = list(d1, d2, d3),
      ##                      c3 = list(d1, d2, d3)),
      ##            b2 = list(c1 = list(d1, d2, d3),
      ##                c2 = list(d1, d2, d3),
      ##                c3 = list(d1, d2, d3)))
      ##        a2 = list(b1 = list(c1 = list(d1, d2, d3),
      ##                      c2 = list(d1, d2, d3),
      ##                      c3 = list(d1, d2, d3)),
      ##            b2 = list(c1 = list(d1, d2, d3),
      ##                c2 = list(d1, d2, d3),
      ##                c3 = list(d1, d2, d3))))


      ## Apply recursively until one matching seq is found in the
      ## list depth. Then for loop in which we once again apply
      ## recursively. When we get to the bottom of both lists, and
      ## side = "lhs", apply fun to seq_lapply(rhs, side =
      ## "rhs"). If side = "rhs", return values.

      ## What about situations where rhs is deeper? How can we add
      ## the depth to resulting list? Specify it in the call, then
      ## replicate
      
      ## Find 
      lseqapply <- function(seq, rhs = NULL, new_seqs = NULL) {
        browser(expr = getOption("debug_on"))

        if (is.list(seq) && seq_id(seq) == ids)
          lapply(seq, lseqapply, rhs = rhs, new_seqs = new_seqs)

        else {
          if (is.null(new_seqs))
            lapply(rhs, lseqapply)
          else
            fun(seq, rhs)
        }
      }
    }

    lapply(a, lseqapply, rhs = b)

    ## else if (!any(in_a)) {
    ##   ids <- c(id_a, id_b)
      
    ##   res <- list()
    ##   for (i in seq_along(b)) {
    ##     res <- c(res, lapply(a, fun, b[[i]]))
    ##   }
    ## }

    ## else if (length(id_b) > length(id_a)) {
    ##   positions <- match(id_b, id_a)
    ##   sorted_id_b <- id_b[order(positions, na.last = NA)]
    ##   ids <- c(sorted_id_b, id_b[!in_a])
    ## }
      
    ## else if (length(id_b) < length(id_a)) {
    ##   browser(expr = getOption("debug_on"))
    ##   ids <- c(id_a, id_b[!in_a])
    ## }

    ## else stop("uh oh")
