library(reshape2)
library(dplyr)
library(ggplot2)
kde=function(sample,h)
{
  x=seq(min(sample),max(sample),length=512)
  Gaussian=density(sample,bw=h,kernel = "gaussian")$y
  Epanechnikov=density(sample,bw=h,kernel = "epanechnikov")$y
  kexp=function(x,sample,h)
  {
    mean(dexp((x-sample)/h))/h
  }
  Exponential=sapply(x, function(x) kexp(x,sample,h))
  d=melt(cbind(Gaussian,Exponential,Epanechnikov)) %>% select(-Var1) %>%
    mutate(sample=rep(x,times=3),h=rep(h,each=n()))
  d=d %>% rename(Kernels=Var2)
  d
}

sample=rnorm(100)
sample=rchisq(100,df=2)
df1=full_join(full_join(kde(sample,0.1),kde(sample,0.2)),kde(sample,0.3))
df2=full_join(full_join(kde(sample,0.4),kde(sample,0.5)),kde(sample,0.6))
df3=full_join(full_join(kde(sample,0.7),kde(sample,0.8)),kde(sample,0.9))
df=full_join(full_join(df1,df2),df3)

ggplot(df,aes(x=sample,y=value,col=Kernels))+geom_line(size=1)+stat_function(fun = dnorm,col="black")+
  facet_wrap(~factor(h),ncol=3)+labs(title = "Kernel density estimates for varying bandwidths")+ 
  coord_cartesian(xlim=c(-3,3)) + theme(plot.title = element_text(hjust = 0.5))

ggplot(df,aes(x=sample,y=value,col=Kernels))+geom_line(size=1)+stat_function(fun = dchisq,
                                                                             args = list(df = 2),
                                                                             col="black")+
  facet_wrap(~factor(h),ncol=3)+labs(title = "Kernel density estimates for varying bandwidths")+ 
  coord_cartesian(xlim=c(-3,3)) + theme(plot.title = element_text(hjust = 0.5))


