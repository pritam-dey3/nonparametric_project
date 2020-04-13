## 4th step: plot the datas generated through other simulations

# After generating the data this function should be compiled
# as it only reads the generated data and plots them
plot_power <- function(n, m, stat, distns, title="", theta_range=c(5, 1, 1000), repl=1000){
  require(lattice)
  data <- matrix(0, nrow=repl, ncol=length(distns))
  colnames(data) <- distns
  for (distn in distns){
    data[, distn] <- as.vector(read.csv(sprintf("data/power -- %s_rejection (%d, %d) -- r%s -- %.2f, %.2f, %.2f -- repl=%d.csv",
                                    stat, n, m, distn, 
                                    theta_range[1], theta_range[2], theta_range[3], repl))[, -1])
  }
  frml = paste(distns, collapse = " + ")
  frml = formula(sprintf("%s ~ theta", frml))
  theta = seq(from = 5, to = 1, length.out = 1000) 
  data = data.frame(data, theta)
  xyplot(frml, data=data,
         auto.key=list(space="right"),
         par.settings=simpleTheme(pch=19),
         xlim=c(0.5, 5.5),
         cex=0.6, type=c("p"), pch=19,
         xlab = "theta", ylab = "Power", main=title)
}

# input the value of n and m, and the distributions you want to plot, the last
# parameter takes what to be written as title in the plot 
plot_power(30, 20, "savage", c("Exp", "Gamma", "Weibull", "Norm", "Logis"), "Savage | m = 20, n = 30")
plot_power(20, 30, "savage", c("Exp", "Gamma", "Weibull", "Norm", "Logis"), "Savage | m = 30, n = 20")
plot_power(30, 30, "savage", c("Exp", "Gamma", "Weibull", "Norm", "Logis"), "Savage | m = 30, n = 30")

plot_power(30, 20, "capon", c("Exp", "Gamma", "Weibull", "Norm", "Logis"), "Capon | m = 20, n = 30")
plot_power(20, 30, "capon", c("Exp", "Gamma", "Weibull", "Norm", "Logis"), "Capon | m = 30, n = 20")
plot_power(30, 30, "capon", c("Exp", "Gamma", "Weibull", "Norm", "Logis"), "Capon | m = 30, n = 30")

## When plotting power curves is done we move on to the next part, plotting qqplots
## to check for normality. Open the fle