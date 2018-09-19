create_dataset <- function(variables, samples) {
  sapply(rnorm(samples), function(x){
    rnorm(variables, mean = x)
  })
}
library("RGCCA")
o <- create_dataset(5000, 250)
i <- create_dataset(5000, 250)

A <- list(o, o)
sgcca.identical <- sgcca(A, matrix(c(0, 1, 1, 0), ncol =2 , nrow =2),
                          c1 = rep(tau.estimate(o), 2),
                          scheme = "centroid")
plot(sgcca.identical$Y[[1]][, 1], sgcca.identical$Y[[2]][, 1])
sgcca.identical$AVE[c("AVE_inner", "AVE_outer")]
o2 <- cor(o, o)
plot(cmdscale(as.dist(o2)), pch = 16)



A <- list(o, t(o))
sgcca.transposed <- sgcca(A, matrix(c(0, 1, 1, 0), ncol =2 , nrow =2),
                        c1 = c(tau.estimate(o), 0.142), # The minimum
                        scheme = "centroid")
plot(sgcca.transposed$Y[[1]][, 1], sgcca.transposed$Y[[2]][, 1])
sgcca.transposed$AVE[c("AVE_inner", "AVE_outer")]
o2 <- cor(o, t(o))
plot(cmdscale(as.dist(o2)), pch = 16)

create_dataset <- function(variables, samples) {
  sapply(rnorm(samples), function(x){
    rnorm(variables, mean = x)
  })
}
set.seed(50)
library("RGCCA")
o <- create_dataset(5000, 500)
i <- create_dataset(5000, 500)
A <- list(t(o), t(i))
C <- matrix(c(0, 1, 1, 0), ncol =2 , nrow =2)
tau <- rep(1/sqrt(500), 2)
sgcca.diff <- sgcca(A, C, tau, scheme = "centroid")
plot(sgcca.diff$Y[[1]][, 1], sgcca.diff$Y[[2]][, 1], pch = 16)
sgcca.diff$AVE[c("AVE_inner", "AVE_outer")]
o2 <- cor(o, i)
plot(cmdscale(as.dist(o2)), pch = 16)
princomp(cbind(sgcca.diff$Y[[1]][, 1], sgcca.diff$Y[[2]][, 1]))
