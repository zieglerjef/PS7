###############################
### Monte Carlo integration ###
###############################

## function takes multiple dimensions and 
## allows for multicore processing (cross-platform)

monteCarlo.int <- function(g,..., lower, upper, n, dimensions, parallelCores=TRUE){
  # create random points 
  u <- runif(n*dimensions, lower, upper)
  # place in matrix w/ each column = dimension
  these <- matrix(u, ncol=dimensions)
  
  x <- apply(these, 1, g,...)
  return(mean(x)*(upper-lower)^dimensions)
}
