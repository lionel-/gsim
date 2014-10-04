
gsim3 <- gsim(ls_subdata, sims3_mclist)
test <- gsim3({
  education %>%
    group_by(country) %>%
    summarise_obs(obs_var = var(.))
})

test <- gsim3({
  resid %>%
    group_by(country) %>%
    summarise_obs(test = mean)
})




microbenchmark(
  vec =   gsim3(I(beta + beta + beta)),
  loop =  gsim3(I(beta %++% beta %++% beta)),
  dplyr = gsim3(I(beta %+% beta %+% beta)),
  times = 5, unit = "s"
)





gsim3 <- gsim(ls_subdata, sims3_mclist)
gsim3({
  x <- x(education)
  data <- cbind(1, x, x^2, 2, 1, 3*x)
})
test <- gsim3(pred <- data %*% beta)
test(2.8)

test <- gsim3(I(beta))

gsim3 <- gsim(ls_subdata, sims3_mclist)
gsim3({
  x <- x(education, n = 5)
  X <- rbind(x, x, x, x, x, x)
})
test <- gsim3(X + beta)

test(2.8)




gsim3 <- gsim(ls_subdata, sims3_mclist)
test <- gsim3(I(cbind(1, x(education, n = 15), 3) %*% rbind(3:1)))

test <- gsim3(I(rbind(1, x(education, n = 15), 3) %*% cbind(3:1)))
test[[1]]

gsim3 <- gsim(ls_subdata, sims3_mclist)
test <- gsim3(I(cbind(1, 4, 3) %*% cbind(3:1)))

gsim3 <- gsim(ls_subdata, sims3_mclist)
test <- gsim3(I(cbind(3:1, 1:3)))
test <- gsim3(I(cbind(3:1)))

test <- gsim3(I(cbind(1, 4, 3)))

rbind(1, 4, 3) %*% cbind(3:1)
cbind(1, 4, 3) %*% cbind(3:1)



test <- gsim3(x <- x(education))
test1 <- gsim3(I(x + x^2))
test2 <- gsim3(I(cbind(1, x, x^2)))
test3 <- gsim3(cbind(1, x, x^2) %*% rbind(2, 3, 1))
test3(2.96)
base::`%*%`(cbind(1, 2.96, 8.7616), rbind(2, 3, 1))


gsim3 <- gsim(ls_subdata, sims3_mclist)

test <- gsim3(cbind(1, x(education, n = 5), 1) %*% rbind(1:3))
test(3)

test <- gsim3(cbind(1:3))

test <- gsim3(I(X))
test <- gsim3({I(beta)})

gsim3 <- gsim(ls_subdata, sims3_mclist)
gsim3({
  X <- cbind(intercept(), education, education^2, age, female, age*education)
  fitted <- X %*% beta
})

test <- gsim3(I(fitted))


gsim3 <- gsim(ls_subdata, sims3_mclist)
gsim3({
  x <- x(education)
  data <- cbind(1, x, x^2, 2, 1, 3*x)
  pred <- data %*% beta
})
test <- gsim3(pred)
test(2.4)

seq_test <- seq(1, 3, len = 100)
plot(seq_test, sapply(seq_test, test))

testy <- gsim3(x + x^2)
testy(1.15)


# Multiple sequences. Add items to list, adjust attributes
gsim3 <- gsim(ls_subdata, sims3_mclist)
test <- gsim3({
  x <- x(education)
  y <- y(seq = c(0, 1))
  (x + y) * x^2
})

gsim3 <- gsim(ls_subdata, sims3_mclist)
test <- gsim3(I(cbind(1, x(education, n = 15), 3)))


test <- gsim3(I(cbind(1, x(education, n = 15), 3) %*% rbind(3:1)))

gsim3 <- gsim(ls_subdata, sims3_mclist)
test <- gsim3(cbind(1, x(education, n = 15), 3) %*% rbind(3:1))

test(0.9)
