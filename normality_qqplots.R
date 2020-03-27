qq <- function(x, y, title="", xl="", yl=""){
  require(ggplot2)
  sx <- sort(x); sy <- sort(y)
  lenx <- length(sx)
  leny <- length(sy)
  if (leny < lenx)sx <- approx(1L:lenx, sx, n = leny)$y
  if (leny > lenx)sy <- approx(1L:leny, sy, n = lenx)$y
  g = ggplot() + geom_point(aes(x=sx, y=sy)) + ggtitle(title) + xlab(xl)
  g 
}

normality_approach <- function(stat){
  require("gridExtra")
  require("ggplotify")
  normal_sample <- rnorm(4000)
  a1 = read.csv(sprintf("data2/simulate_%s -- n-5, 10, 15, 20, 25, 30 -- lbd-1.0.csv", stat))[,-1]
  a2 = read.csv(sprintf("data2/simulate_%s -- n-5, 10, 15, 20, 25, 30 -- lbd-2.0.csv", stat))[,-1]
  a3 = read.csv(sprintf("data2/simulate_%s -- n-5, 10, 15, 20, 25, 30 -- lbd-0.5.csv", stat))[,-1]
  
  p1 = sapply(colnames(a1), function(n) as.grob(qq(a1[,n], normal_sample, title = "lambda = 1", xl=n)))
  p2 = sapply(colnames(a2), function(n) as.grob(qq(a2[,n], normal_sample, title = "lambda = 2", xl=n)))
  p3 = sapply(colnames(a3), function(n) as.grob(qq(a3[,n], normal_sample, title = "lambda = 0.5", xl=n)))
  
  g = arrangeGrob(grobs = c(p1, p2, p3), nrow=3, ncol=6)
  ggsave("plots/qqplots.pdf", g, units = "in", height = 8, width = 16)
}


a = normality_approach("capon")

a = read.csv("data2/simulate_capon -- n-5, 10, 15, 20, 25, 30 -- lbd-1.0.csv")[,-1]
b = qq(scale(a[,1], TRUE, TRUE), normal_sample, "hulala")
plot(b)