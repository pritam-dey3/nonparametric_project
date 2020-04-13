# modified functions to draw samples
# this functions should be compiled at the first step
rExp <- function(n, theta) {
  rexp(n, rate = 1/theta)
}

rGamma <- function(n, theta, shape=0.5) {
  rgamma(n, shape = shape, scale = theta)
}

rWeibull <- function(n, theta, shape=0.5){
  rweibull(n, shape = shape, scale = theta)
}

rNorm <- function(n, theta=1) {
  rnorm(n, mean = 0, sd = theta)
}

rCauchy <- function(n, theta, location=0.5){
  rcauchy(n, location = location, scale = theta)
}

rLogis <- function(n, theta, location=0.5){
  rlogis(n, location = location, scale = theta)
}

# after compiling these functions go ahead and compile the functions of capon.R and
# savage.R
