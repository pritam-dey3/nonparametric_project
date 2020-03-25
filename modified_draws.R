#modified functions to draw samples
rExp <- function(n, theta=1) {
  rexp(n, rate = 1/theta)
}

rGamma <- function(n, theta=1, shape=0.5) {
  rgamma(n, shape = shape, scale = theta)
}

rWeibull <- function(n, theta=1, shape=0.5){
  rweibull(n, shape = shape, scale = theta)
}

rNorm <- function(n, theta=1) {
  rnorm(n, mean = 0, sd = theta)
}

rCauchy <- function(n, theta=1, shape=0.5){
  rcauchy(n, location = shape, scale = theta)
}

rLogis <- function(n, theta, shape=0.5){
  rlogis(n, location = shape, scale = theta)
}
