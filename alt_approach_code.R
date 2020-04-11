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

library(reshape2)
library(dplyr)
library(ggplot2)

df=melt(cbind(Exponential,Gamma,Weibull)) %>% select(-Var1) %>% mutate(theta = rep(theta,times=3)) %>%
  rename(Distribution = Var2)
ggplot(df,aes(x=theta,y=value,col=Distribution))+geom_line()+facet_grid(~Distribution,scales = "free")+
  labs(title = "Power function for Mann Whitney Statistic", y = "Power")+
  theme(plot.title = element_text(hjust = 0.5))

