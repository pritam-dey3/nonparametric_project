plot_power <- function(n, m, stat, distns){
  require(lattice)
  data <- matrix(0, nrow=1000, ncol=length(distns))
  colnames(data) <- distns
  for (distn in distns){
    data[, distn] <- as.vector(read.csv(sprintf("data2/power -- %s_rejection (%d, %d) -- r%s -- 0.20, 1.00, 1000.00 -- repl=1000.csv",
                                    stat, n, m, distn))[, -1])
  }
  frml = paste(distns, collapse = " + ")
  frml = formula(sprintf("%s ~ theta", frml))
  print(frml)
  theta = seq(from = 0.2, to = 1, length.out = 1000)
  data = data.frame(data, theta)
  xyplot(frml, data=data, 
         auto.key=TRUE, xlim=c(1.05, 0.15),
         cex=0.6, type=c("p"), pch=19)
}

plot_power(30, 30, "capon", c("Norm", "Logis", "Weibull", "Gamma", "Exp"))
xyplot(Norm + Logis + Weibull + Gamma + Exp ~ theta, data=a, 
      auto.key=TRUE, xlim=c(1.05, 0.15),
     cex=0.6, type=c("p"), pch=19)
