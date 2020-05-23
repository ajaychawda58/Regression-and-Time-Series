
ex_1 <- read.table('example1.txt', header=FALSE, sep=',',col.names=c('X','Y'))
ex_2 <- read.table('example2.txt', header=FALSE, sep=',',col.names=c('X','Y'))
ex_3 <- read.table('example3.txt', header=FALSE, sep=',',col.names=c('X','Y'))
ex_4 <- read.table('example4.txt', header=FALSE, sep=',',col.names=c('X','Y'))
ex_5 <- read.table('example5.txt', header=FALSE, sep=',',col.names=c('X','Y'))

graphic_plot <- function(X,Y){
  y_n = mean(Y)
  x_n = mean(X)
  
  b2 = sum((Y - y_n)*(X-x_n))/sum((X-x_n)^2)
  b1 = (y_n - (b2*x_n))
  
  
  
  residuals <- (Y-(b1+b2*X))
  
  
  hat_matrix <- X%*%(solve(t(X)%*%X)%*%t(X))
  diag_hat_matrix <- diag(hat_matrix)
  squared_residuals <- (residuals^2)
  
  pdf(file="Graphics.pdf")
  par(mfrow=c(2,2))
  plot(X,Y, main='Scatter Plot with Regression Line')
  lines(X,b1+b2*X,col='red',lwd=2)
  
  plot(residuals, main='Residuals')
  
  qqnorm(residuals, pch = 1, frame = FALSE)
  qqline(residuals, col = "steelblue", lwd = 2)
  plot(diag_hat_matrix,squared_residuals,main='Hat Matrix vs Squared Residuals' )
  dev.off()
  
}

graphic_plot(ex_1$X,ex_1$Y)
graphic_plot(ex_2$X,ex_2$Y)
graphic_plot(ex_3$X,ex_3$Y)
graphic_plot(ex_4$X,ex_4$Y)
graphic_plot(ex_5$X,ex_5$Y)
