create_dataset <- function(variables, samples) {
  vapply(rnorm(samples), function(x){
    rnorm(variables, mean = x)
  }, numeric(variables))
}

set.seed(1001)
library("RGCCA")
library("fitdistrplus")
o <- t(create_dataset(5000, 250))
i <- t(create_dataset(5000, 250))

shrinkage <- c(o = tau.estimate(o), i = tau.estimate(i))
# Testing identical ####
A <- list(o, o)
model0 <- matrix(c(0, 1, 1, 0), ncol = 2 , nrow = 2)
## Testing sgcca ####
sgcca.identical <- sgcca(A, model0,
                          c1 = rep(shrinkage["o"], 2),
                          scheme = "centroid", verbose = FALSE)
plot(sgcca.identical$Y[[1]][, 1], sgcca.identical$Y[[2]][, 1])
sgcca.identical$AVE[c("AVE_inner", "AVE_outer")]
hist(sgcca.identical$a[[1]][, 1])
hist(sgcca.identical$a[[2]][, 1])


## Testing rgcca ####
rgcca.identical <- rgcca(A, model0,
                         tau = rep(shrinkage["o"], 2),
                         scheme = "centroid", verbose = FALSE)
plot(rgcca.identical$Y[[1]][, 1], rgcca.identical$Y[[2]][, 1])
rgcca.identical$AVE[c("AVE_inner", "AVE_outer")]
hist(rgcca.identical$a[[1]][, 1])
fitdistr(rgcca.identical$a[[1]][, 1], "normal")
hist(rgcca.identical$a[[2]][, 1])
fitdistr(rgcca.identical$a[[2]][, 1], "normal")

## Comparing rgcca to sgcca ####

# Same differences between first to first and second to second Y
plot(rgcca.identical$Y[[1]][, 1], sgcca.identical$Y[[1]][, 1])
plot(rgcca.identical$Y[[2]][, 1], sgcca.identical$Y[[2]][, 1])
# However the weights of each variable are completely different!
# In sgcca it depends on just two variables and in rgcca in all of them.
# This might be due to the assumption of sparsity of the data??
plot(rgcca.identical$a[[1]][, 1], sgcca.identical$a[[1]][, 1])
plot(rgcca.identical$a[[2]][, 1], sgcca.identical$a[[2]][, 1])


# Different dataset ####
B <- list(o, i)
## Testing sgcca ####
sgcca.diff <- sgcca(A, model0,
                         c1 = shrinkage,
                         scheme = "centroid", verbose = FALSE)
plot(sgcca.diff$Y[[1]][, 1], sgcca.diff$Y[[2]][, 1])
sgcca.diff$AVE[c("AVE_inner", "AVE_outer")]
hist(sgcca.diff$a[[1]][, 1])
hist(sgcca.diff$a[[2]][, 1])


## Testing rgcca ####
rgcca.diff <- rgcca(B, model0,
                         tau = shrinkage,
                         scheme = "centroid", verbose = FALSE)
plot(rgcca.diff$Y[[1]][, 1], rgcca.identical$Y[[2]][, 1])
rgcca.diff$AVE[c("AVE_inner", "AVE_outer")]
# Despite having a great correlation the inner AVE reports low values!
# It indicates that the model is not good!!
# Compare this to the sgcca.diff: the assumptions might be too much for this data?
hist(rgcca.diff$a[[1]][, 1])
fitdistr(rgcca.diff$a[[1]][, 1], "normal")
hist(rgcca.diff$a[[2]][, 1])
fitdistr(rgcca.diff$a[[2]][, 1], "normal")

# Different distributions between the first and the second dimension!
plot(rgcca.diff$Y[[1]][, 1], sgcca.diff$Y[[1]][, 1])
plot(rgcca.diff$Y[[2]][, 1], sgcca.diff$Y[[2]][, 1])
# However the weights of each variable are completely different!
# In sgcca it depends on just two variables and in rgcca in all of them.
# This might be due to the assumption of sparsity of the data??
plot(rgcca.diff$a[[1]][, 1], sgcca.diff$a[[1]][, 1])
plot(rgcca.diff$a[[2]][, 1], sgcca.diff$a[[2]][, 1])


# mvrnorm ####
library("MASS")
Sigma <- matrix(1, ncol = 5000, nrow = 500)
o2 <- mvrnorm(n = 250, mu = rep(0, 5000), Sigma = 1, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
