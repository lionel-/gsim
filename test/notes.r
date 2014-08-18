

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
