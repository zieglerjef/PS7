# load libraries
library(SparseGrid)
library(cubature)
library(foreach)
library(doParallel)
library(testthat)
library(microbenchmark)
library(mvtnorm)

# set working directory
setwd("~/Documents/Git/Class/PS7")

#############################
### Numerical integration ###
#############################

# execute sg.int.R file
source("sg.int.R")

####################
### Unit testing ###
####################

# execute testThat.R file
test_file("testThat.R")

#################
### Benchmark ###
#################

## measure speed depending on 1) number of dimensions and
## 2) number of cores

# create test function (found in adaptIntegrate help file)
M_2_SQRTPI <- 2/sqrt(pi)
testFn1 <- function(x) {
  scale = 1.0
  val = 0
  dim = length(x)
  val = sum (((1-x) / x)^2)
  scale = prod(M_2_SQRTPI/x^2)
  exp(-val) * scale
}

# run sg.int function with three-dimensions, w/ and w/o parallelCores
microbenchmark(sg.int(testFn1, lower=rep(0,3), upper=rep(1,3), parallelCores=TRUE), times=100)
microbenchmark(sg.int(testFn1, lower=rep(0,3), upper=rep(1,3), parallelCores=FALSE), times=100)

# create test function (found in adaptIntegrate help file)
testFn2 <- function(x) {
  a = (1+sqrt(10.0))/9.0
  prod(a/(a+1)*((a+1)/(a+x))^2)
}

# run sg.int function with five-dimensions, w/ and w/o parallelCores
microbenchmark(sg.int(testFn2, lower=rep(0,5), upper=rep(1,5), parallelCores=TRUE), times=100)
microbenchmark(sg.int(testFn2, lower=rep(0,5), upper=rep(1,5), parallelCores=FALSE), times=100)

####################################
### Comparison w/ adaptIntegrate ###
####################################

### accuracy ###
# create test function (multivariate normal)
accuracyTest <- function(x){
  dmvnorm(x, mean=rep(0, 2), sigma=diag(rep(1, 2)))
}
# correct answer
correctAnswer <- as.numeric(pmvnorm(upper=rep(.5, 2), mean=rep(0, 2), sigma=diag(rep(1, 2))))

abs(adaptIntegrate(accuracyTest, lowerLimit=rep(-100, 2), upperLimit=rep(.5, 2))$integral - correctAnswer)
abs(sg.int(accuracyTest, lower=rep(-100,2), upper=rep(0.5, 2)) - correctAnswer)

## adaptIntegrate more accurate

### speed ###
microbenchmark(adaptIntegrate(testFn1, rep(0,3), rep(1,3)), times=100)
microbenchmark(sg.int(testFn1, lower=rep(0,3), upper=rep(1,3), parallelCores=FALSE), times=100)

microbenchmark(adaptIntegrate(testFn2, rep(0,5), rep(1,5)), times=100)
microbenchmark(sg.int(testFn2, lower=rep(0,5), upper=rep(1,5), parallelCores=FALSE), times=100)

## sg.int is faster, but this is largely a function of the tolerance and maxEval defaults

###############################
### Monte Carlo integration ###
###############################

# execute sg.int.R file
source("monteCarlo.int.R")

# execute testThat.R file
test_file("testThat.mc.R")
# not very accurate

# test monteCarlo.int with accuracyTest
monteCarlo.int(accuracyTest, lower = -4, upper = .5, n=100000, dimensions=2) - correctAnswer 
