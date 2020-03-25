library(EnvStats)
library(pbapply)
library(lattice)

theta=seq(1,3,len=1000)
rejection=function(n,m,rdist,theta=1,repl=10^3,p=0.05)
{
 new=function()
  {
   x=rdist(n,1)
   y=rdist(m,theta)
   twoSampleLinearRankTest(x,y,location.shift.null = 0,scale.shift.null = 1,alternative = "less",
                  test = "wilcoxon",shift.type = "scale")$p.value
 }
 s=replicate(repl,new())
 length(which(s<p))/repl
}
c1=pbsapply(theta,function(i) rejection(30,30,rExp,theta=i))
xyplot(c~theta,grid=TRUE,type="l",xlab="theta",
       ylab="power",ylim = c(0,1),main="Exponential Distribution")
c2=pbsapply(theta,function(i) rejection(30,30,rGamma,theta=i))
xyplot(c~theta,grid=TRUE,type="l",xlab="theta",
       ylab="power",ylim = c(0,1),main="Gamma Distribution")
c3=pbsapply(theta,function(i) rejection(30,30,rWeibull,theta=i))
xyplot(c~theta,grid=TRUE,type="l",xlab="theta",
       ylab="power",ylim = c(0,1),main="Weibull Distribution")