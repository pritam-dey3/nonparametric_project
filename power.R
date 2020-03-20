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
