#############################
### Numerical integration ###
#############################

# load libraries
library(SparseGrid)
library(cubature)
library(doParallel)
library(testthat)
library(microbenchmark)

## alter function to take multiple dimensions and 
## adjust the number of cores used in multicore processing (cross-platform)

# begin numerical integration function
sg.int <- function(g,..., lower, upper, dimensions, parallelCores=1){
  # allows the function to have parallel processing
  # doParellel allows for multicore functionality even w/ Windows
  registerDoParallel(cores=parallelCores)
  # round down at lower threshold
  lower <- floor(lower)
  # round up at uppper threshold
  upper <- ceiling(upper)
  # throw error if lower is larger than upper
  if (any(lower > upper)) stop("Lower must be smaller than upper.")
  # apply the function to all the dimensions specified by user, create vector each iteration
  # create data frame from all combination of vectors and turn into matrix
  gridss <- as.matrix(expand.grid(unlist(lapply(1:dimensions, function(i) seq(lower[i], upper[i]-1, by=1)))))
  # create nodes an weights to be used for integration from SpareGrid package
  # allow for multiple dimensions set by user
  sp.grid <- createIntegrationGrid('KPU', dimension=dimensions, k=5)
  # assign matrix w/ nodes by row from grid object
  nodes <- gridss[1,] + sp.grid$nodes
  # assign weights from grid object
  weights <- sp.grid$weights

    for (i in 2:nrow(gridss)) {
      # combine nodes by row
      nodes <- rbind(nodes, gridss[i,] + sp.grid$nodes)  
      # concatenate weights
      weights <- c(weights, sp.grid$weights)
    }
  # apply function to nodes matrix
  gx.sp <- apply(nodes, 1, g,...)
  # matrix multiply nodes by weights
  val.sp <- gx.sp %*% weights
  # return product
  val.sp
}

####################
### Unit testing ###
####################

#################
### Benchmark ###
#################

## measure speed depending on 1) number of dimensions and
## 2) number of cores

# create test function
testFn0 <- function(x) {
  prod(cos(x))
}

# run sg.int function with three-dimensions, comparing 2 versus 4
microbenchmark(sg.int(testFn0, lower=rep(0,3), upper=rep(1,3), dimensions=3, parallelCores=2), times=100)
microbenchmark(sg.int(testFn0, lower=rep(0,3), upper=rep(1,3), dimensions=3, parallelCores=4), times=100)

# run sg.int function with five-dimensions, comparing 2 versus 4
microbenchmark(sg.int(testFn0, lower=rep(0,5), upper=rep(1,5), dimensions=5, parallelCores=2), times=100)
microbenchmark(sg.int(testFn0, lower=rep(0,5), upper=rep(1,5), dimensions=5, parallelCores=4), times=100)