library(lattice)

`%c%` <- function(x, y) paste(x, y, sep = "")

#size: sample size
#h: bandwidth
kde=function(size, h, dist='norm')
{
  #create sampling and density functions
  rdist = get('r' %c% dist)
  ddist = get('d' %c% dist)
  #draw sample
  sample = rdist(size)
  #data points
  data.points = seq(min(sample),max(sample),length=512)
  #gausian kernel estimation
  Estimate_normal = density(sample,bw=h,kernel = "gaussian")$y
  #optimal kernel estimation
  Estimate_optimal = density(sample,bw=h,kernel = "epanechnikov")$y
  #exponential kernel estimation
  kexp = function(x,sample,h)
  {
    mean(dexp((x-sample)/h))
  }
  Estimate_exponential = sapply(data.points, function(x) kexp(x,sample,h))
  
  #true density
  True_density=ddist(data.points)
  
  xyplot(True_density + Estimate_normal + Estimate_exponential + Estimate_optimal ~ data.points,
         grid=TRUE, 
         auto.key = TRUE, main="Comparison between true density and 
         kernel density estimates", xlab="Sample", ylab="Density",type="l",lwd=3)
}

kde(100, 0.5)
kde(100, 0.5, 'logis')
