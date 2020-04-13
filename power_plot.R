# plot the datas generated through other simulations

# After generating the data this function should be compiled
# as it only reads the generated data and plots them
plot_power <- function(n, m, stat, distns, title=""){
  require(lattice)
  data <- matrix(0, nrow=1000, ncol=length(distns))
  colnames(data) <- distns
  for (distn in distns){
    data[, distn] <- as.vector(read.csv(sprintf("data/power -- %s_rejection (%d, %d) -- r%s -- 5.00, 1.00, 1000.00 -- repl=1000.csv",
                                    stat, n, m, distn))[, -1])
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

