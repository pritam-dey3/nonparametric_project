library(ggplot2)
library(dplyr)
library(reshape)

kde=function(sample,h)
{
  d = density(sample,bw=h,kernel = "gaussian")
  Gaussian = d$y
  sample.values = d$x
  Epanechnikov = density(sample,bw=h,kernel = "epanechnikov")$y
  kexp=function(x,sample,h)
  {
    mean(dexp((x-sample)/h))/h
  }
  Exponential=sapply(sample.values, function(x) kexp(x, sample,h))
  d = melt(cbind(Gaussian,Exponential,Epanechnikov)) %>% mutate(h=rep(h,each=n()),sample=rep(sample.values,times=3))
  d
}

sample = rnorm(100)
df = rbind(kde(sample,0.1),kde(sample,0.2),kde(sample,0.3),kde(sample,0.4),kde(sample,0.5),
         kde(sample,0.6),kde(sample,0.7),kde(sample,0.8),kde(sample,0.9)) %>% select(-X1) %>% dplyr::rename(Kernels = X2)
                                                                                        
ggplot(df,aes(x=sample,y=value,col=Kernels)) + geom_line(size=1)
  + stat_function(fun = dnorm, col="black") +
  facet_wrap(~factor(h),ncol=3,scales="free") +
  labs(title = "Kernel density estimates for varying bandwidths",y="Density") + 
  coord_cartesian(xlim=c(-3,3)) + theme(plot.title = element_text(hjust = 0.5))


sample = rchisq(100,df=2)
ggplot(df,aes(x=sample,y=value,col=Kernels))+geom_line(size=1)+stat_function(fun = dchisq,
                                                                             args = list(df = 2),
                                                                             col="black")+
  facet_wrap(~factor(h),ncol=3,scales="free")+
  labs(title = "Kernel density estimates for varying bandwidths",y="Density")+ 
  coord_cartesian(xlim=c(0, 3)) + theme(plot.title = element_text(hjust = 0.5))
