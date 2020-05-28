confidence_band <- function(X,b,gamma,sigma){
  
  N <- length(X)
  d <- length(b)
  #From Theorem 2.7.2
  
  s_hat_k <- sigma^2 * (solve(t(X)%*%X))
  conf_band <- sqrt(qf((gamma),d,N-d)*s_hat_k*d)
  
  #Simulate Y_i
  Y_i <- b[1] + b[2]*X + b[3]*X^2 + rnorm(25,0,sigma)
  #print(Y_i)
  b_hat <- b[1] + b[2]*X + b[3]*X^2
  
  CI <- cbind((b_hat-conf_band), (b_hat+conf_band))
  
  #plot simulated values
  plot(X, Y_i, col = "black", main = "Confidence Band", xlim=c(-2,4),ylim=c(0,13))
  range_x <- seq(from=-2,to=4,by=(6/24))
  #quadratic regression line
  lines(range_x,b_hat,col="red",lwd=1)
  #plot calculated confidence band
  lines(range_x,CI[,1],col="blue",lty=2,lwd=2)
  lines(range_x,CI[,2],col="blue",lty=2,lwd=2)
  
  print(Y_i)
  print(b_hat)
  print(conf_band)
  print(CI)
  
}

#randomnly generated uniformly distributed random variables
X <- runif(25, min=-2, max=4)

#parameters of b
b1 <- 3
b2 <- -2
b3 <- 1
b <- c(b1,b2,b3)

#sigma
sigma <- 2
#significance level
gamma <- 0.95

confidence_band(X,b,gamma,sigma)


