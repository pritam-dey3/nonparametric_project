#power function simulation
library(lattice)

power_curve <- function(n, m, from=0.2, to=1, len=30, 
                        rdist=rNorm, stat_rejection = capon_rejection,
                        repl=500)
{
  #progress bar
  pb <- txtProgressBar(style = 3, min = from, max = to, initial = from)
  #theta sequence
  theta <- seq(from, to, length.out = len)
  power <- sapply(theta, function(i){ 
    setTxtProgressBar(pb, i) #pb
    stat_rejection(n, m, rdist = rdist, theta = i, repl = repl)
  })
  write.csv(power, sprintf("data/power -- %s (%.0f, %.0f) -- %s -- %.2f, %.2f, %.2f -- repl=%0.f.csv",
            as.character(substitute(stat_rejection)), n, m,
            as.character(substitute(rdist)), from, to, len, repl))
  close(pb) #pb
  xyplot(power ~ theta, xlim = c(to + 0.2, from - 0.2))
}

power_curve(30, 30, rdist = rExp, len=50, stat_rejection = savage_rejection, repl=250)
power_curve(30, 30, len = 50, rdist = rWeibull, stat_rejection = savage_rejection, repl=250)
power_curve(30, 30, len = 50, rdist = rGamma, stat_rejection = savage_rejection, repl=250)
power_curve(30, 30, len = 50, rdist = rNorm, stat_rejection = savage_rejection, repl=250)

#get the data and plot
gamma.savage.3030 <- read.csv("data/power -- savage_rejection (30, 30) -- rGamma -- 0.20, 1.00, 50.00 -- repl=250.csv")[,-1]
exp.savage.3030 <- read.csv("data/power -- savage_rejection (30, 30) -- rExp -- 0.20, 1.00, 50.00 -- repl=250.csv")[,-1]
norm.savage.3030 <- read.csv("data/power -- savage_rejection (30, 30) -- rNorm -- 0.20, 1.00, 50.00 -- repl=250.csv")[,-1]
weibull.savage.3030 <- read.csv("data/power -- savage_rejection (30, 30) -- rWeibull -- 0.20, 1.00, 50.00 -- repl=250.csv")[,-1]

power_mat <- data.frame(gamma.savage.3030, exp.savage.3030, norm.savage.3030, weibull.savage.3030, theta = seq(0.20, 1, length.out = 50))
xyplot(exp.savage.3030 + gamma.savage.3030 + weibull.savage.3030 + norm.savage.3030 ~ theta, data = power_mat, 
       xlim = c(1 + 0.2, 0.2 - 0.2),
       auto.key = TRUE,
       type = c("b"), cex=1.2)


### for capon
power_curve(30, 30, len = 50, rdist = rExp, stat_rejection = capon_rejection, repl=250)
power_curve(30, 30, len = 50, rdist = rWeibull, stat_rejection = capon_rejection, repl=250)
power_curve(30, 30, len = 50, rdist = rGamma, stat_rejection = capon_rejection, repl=250)
power_curve(30, 30, len = 50, rdist = rNorm, stat_rejection = capon_rejection, repl=250)

#get the data and plot
gamma.capon.3030 <- read.csv("data/power -- capon_rejection (30, 30) -- rGamma -- 0.20, 1.00, 50.00 -- repl=250.csv")[,-1]
exp.capon.3030 <- read.csv("data/power -- capon_rejection (30, 30) -- rExp -- 0.20, 1.00, 50.00 -- repl=250.csv")[,-1]
norm.capon.3030 <- read.csv("data/power -- capon_rejection (30, 30) -- rNorm -- 0.20, 1.00, 50.00 -- repl=250.csv")[,-1]
weibull.capon.3030 <- read.csv("data/power -- capon_rejection (30, 30) -- rWeibull -- 0.20, 1.00, 50.00 -- repl=250.csv")[,-1]

power_mat <- data.frame(gamma.capon.3030, exp.capon.3030, norm.capon.3030, weibull.capon.3030, theta = seq(0.20, 1, length.out = 50))
xyplot(exp.capon.3030 + gamma.capon.3030 + weibull.capon.3030 + norm.capon.3030 ~ theta, data = power_mat, 
       xlim = c(1 + 0.2, 0.2 - 0.2),
       auto.key = TRUE,
       type = c("b"), cex=1.2)
