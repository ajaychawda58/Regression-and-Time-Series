least_square <- function(X,Y){
  y_n = mean(Y)
  x_n = mean(X)
  
  b2 = sum((Y - y_n)*(X-x_n))/sum((X-x_n)^2)
  b1 = (y_n - (b2*x_n))
  
  plot(X,Y)
  lines(X,b1+b2*X,col='red',lwd=2)
}
