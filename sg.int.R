#############################
### Numerical integration ###
#############################

## alter function to take multiple dimensions and 
## adjust the number of cores used in multicore processing (cross-platform)

# begin numerical integration function
sg.int <- function(g,..., lower, upper, parallelCores=TRUE){
  # round down at lower threshold
  lower <- floor(lower)
  # round up at uppper threshold
  upper <- ceiling(upper)
  # throw error if lower is larger than upper
  if (any(lower > upper)) stop("Lower must be smaller than upper.")
  # set the number of dimensions based on the length of the lower vector
  dimensions <- length(lower)
  # apply the function to all the dimensions specified by user, create vector each iteration
  # create data frame from all combination of vectors and turn into matrix
  gridss <- as.matrix(expand.grid(unlist(lapply(1:dimensions, function(i) seq(lower[i], upper[i]-1, by=1)))))
  if(parallelCores==T){
    # detect the number of cores
    nCores <- detectCores() - 1
    # initiate cluster
    cl <- makePSOCKcluster(nCores)
    registerDoParallel(cl)
    # create data frame from all combination of vectors and turn into matrix
    gridss <- as.matrix(expand.grid(unlist(foreach(i = 1:dimensions, .combine = c)
                                           %dopar% seq(lower[i], upper[i]-1, by=1))))
    stopCluster(cl)
  }
  # create nodes an weights to be used for integration from SpareGrid package
  # allow for multiple dimensions set by user
  sp.grid <- createIntegrationGrid('KPU', dimension=dimensions, k=5)
  # assign matrix w/ nodes by row from grid object
  nodes <- gridss[1,] + sp.grid$nodes
  # assign weights from grid object
  weights <- sp.grid$weights
  for (i in 2:nrow(gridss)){
    # combine nodes by row
    nodes <- rbind(nodes, gridss[i,] + sp.grid$nodes)  
    # concatenate weights
    weights <- c(weights, sp.grid$weights)
  }
  # apply function to nodes matrix
  gx.sp <- apply(nodes, 1, g,...)
  # matrix multiply nodes by weights
  val.sp <- (gx.sp %*% weights)/dimensions
  # return product
  val.sp
}