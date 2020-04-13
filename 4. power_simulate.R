## 3rd step: power function simulation
## after generating the data power_plot.R (4th step) file contains functions to plot those data

# this function generates the power for given n and m, for a range of values of theta
# for a particular distribution for a particular statistic
# then it saves the generated data in the data folder
# data folder should be created on the working directory before running this function
power_curve <- function(n, m, from=5, to=1, len=1000, 
                        rdist=rNorm, stat_rejection = capon_rejection,
                        repl=1000)
{
  require(lattice)
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
  xyplot(power ~ theta, xlim = c(to - 0.2, from + 0.2))
}

# this functions should be run in order to generate data
# 5 distributions, 3 different pair of (n, m), 2 statistic
# total 30 files should be generated
power_curve(30, 30, len = 1000, rdist = rWeibull, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rNorm, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rExp, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rGamma, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rLogis, stat_rejection = savage_rejection, repl=1000)

power_curve(30, 20, len = 1000, rdist = rWeibull, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rNorm, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rExp, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rGamma, stat_rejection = savage_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rLogis, stat_rejection = savage_rejection, repl=1000)

power_curve(20, 30, len = 1000, rdist = rWeibull, stat_rejection = savage_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rNorm, stat_rejection = savage_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rExp, stat_rejection = savage_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rGamma, stat_rejection = savage_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rLogis, stat_rejection = savage_rejection, repl=1000)



power_curve(30, 30, len = 1000, rdist = rWeibull, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rNorm, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rExp, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rGamma, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 30, len = 1000, rdist = rLogis, stat_rejection = capon_rejection, repl=1000)

power_curve(30, 20, len = 1000, rdist = rWeibull, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rNorm, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rExp, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rGamma, stat_rejection = capon_rejection, repl=1000)
power_curve(30, 20, len = 1000, rdist = rLogis, stat_rejection = capon_rejection, repl=1000)

power_curve(20, 30, len = 1000, rdist = rWeibull, stat_rejection = capon_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rNorm, stat_rejection = capon_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rExp, stat_rejection = capon_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rGamma, stat_rejection = capon_rejection, repl=1000)
power_curve(20, 30, len = 1000, rdist = rLogis, stat_rejection = capon_rejection, repl=1000)



