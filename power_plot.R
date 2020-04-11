# plot the datas generated 

plot_power <- function(n, m, stat, distns, title=""){
  require(lattice)
  data <- matrix(0, nrow=1000, ncol=length(distns))
  colnames(data) <- distns
  for (distn in distns){
    data[, distn] <- as.vector(read.csv(sprintf("data2/power -- %s_rejection (%d, %d) -- r%s -- 0.20, 1.00, 1000.00 -- repl=1000.csv",
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

plot_power(20, 30, "capon", c("Exp", "Gamma", "Weibull", "Norm", "Logis"), "Capon's | m = 30, n = 20")
xyplot(Norm + Logis + Weibull + Gamma + Exp ~ theta, data=a, 
      key=list(space="right", text=distns), xlim=c(0.05, 5.95),
     cex=0.6, type=c("p"), pch=19)
