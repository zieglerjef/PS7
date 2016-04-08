###############################
### Monte Carlo integration ###
###############################

## function takes multiple dimensions and 
## allows for multicore processing (cross-platform)

mcInt <- function(ftn, a, b, n, dimensions){
  u <- runif(n*dimensions, a, b)
  these <- matrix(u, ncol=dimensions)
  x <- apply(these,1, ftn)
  return(mean(x)*(b-a)^dimensions)
}

mcInt(myNorm, -4, .5, n=100000, dimensions=2)-ans 

