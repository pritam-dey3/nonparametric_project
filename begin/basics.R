library(lattice)
library(gridExtra)

n=10^2
theta=1.5

#distributions with R as support -- normal and logistic
s=sort(rNorm(n))
x=pnorm(s)
y=pnorm(s*theta)
p1=xyplot(x+y~1:n,grid=TRUE,type="l",auto.key = TRUE,xlab = "value of the sample",ylab = "F(.)",
       main="Normal Distribution")
s=sort(rLogis(n,1,0))
x=plogis(s)
y=plogis(s*theta)
p2=xyplot(x+y~1:n,grid=TRUE,type="l",auto.key = TRUE,xlab = "value of the sample",ylab = "F(.)",
          main="Logistic Distribution")
grid.arrange(p1,p2,ncol=2)

#distributions with only positive support
s=sort(rExp(n))
x=pexp(s)
y=pexp(s*theta)
p1=xyplot(x+y~1:n,grid=TRUE,type="l",auto.key = TRUE,xlab = "value of the sample",ylab = "F(.)",
          main="Exponential Distribution")
s=sort(rGamma(n))
x=pgamma(s,0.5)
y=pgamma(s*theta,0.5)
p2=xyplot(x+y~1:n,grid=TRUE,type="l",auto.key = TRUE,xlab = "value of the sample",ylab = "F(.)",
          main="Gamma Distribution")
s=sort(rWeibull(n))
x=pweibull(s,0.5)
y=pweibull(s*theta,0.5)
p3=xyplot(x+y~1:n,grid=TRUE,type="l",auto.key = TRUE,xlab = "value of the sample",ylab = "F(.)",
          main="Weibull Distribution")
grid.arrange(p1,p2,p3,ncol=3)
