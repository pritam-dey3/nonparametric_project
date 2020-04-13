## 4th step: plotting power of mann whitney statistics for positive valued random variables

library(EnvStats)
library(pbapply)
library(lattice)

theta = seq(1,3,len=1000)
rejection=function(n, m, rdist, theta=1, repl=10^3, p=0.05)
{
  new=function()
  {
    x=rdist(n,1)
    y=rdist(m,theta)
    twoSampleLinearRankTest(x, y,location.shift.null = 0,scale.shift.null = 1,alternative = "less",
                            test = "wilcoxon", shift.type = "scale")$p.value
  }
  s=replicate(repl,new())
  length(which(s<p))/repl
}

Exponential = pbsapply(theta,function(i) rejection(30,30,rExp,theta=i))
Gamma = pbsapply(theta,function(i) rejection(30,30,rGamma,theta=i))
Weibull = pbsapply(theta,function(i) rejection(30,30,rWeibull,theta=i))

xyplot(Exponential + Gamma + Weibull ~ theta,
       auto.key=list(space="right"), xlim=c(0.05, 3.95),
       cex=0.6, type=c("p"), pch=19,
       par.settings=simpleTheme(pch=19),
       ylab="Power")