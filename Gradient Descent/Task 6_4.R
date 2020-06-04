#gradient descent function
gradient_descent <- function(X,Y, learning_rate,Threshold,n,max_iter){
  
  
  b1 <- runif(1,0,1)
  b2 <- runif(1,0,1)
  x <- seq(from = 0 , to = 1, by= 1/(n-1))
  y_hat <- b1 + exp(b2*X)
  MSE <- sum((y_hat - Y)^2)/n
  converged = 0
  iterations = 0
  #converging to minimum
  while(converged == 0){
    b1_new <- b1 - learning_rate * (1/n * sum(y_hat - Y))
    b2_new <- b2 - learning_rate * (1/n * sum((y_hat - Y)* (X)*(exp(b2*X))))
    
    b1 <- b1_new
    b2 <- b2_new
    y_hat <- b1 + exp(b2*X)
    #comparing loss 
    MSE_new <- sum((Y - y_hat) ^ 2) / n
    if(MSE - MSE_new <= Threshold){
      plot(X,Y, ylim = c(0,10))
      lines(x,b1+exp(b2*x)) 
      converged = 1
      print(paste("Optimal intercept:", b1, "Optimal slope:", b2))
      return(c(b1,b2))
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      plot(X,Y, ylim = c(0,10))
      lines(x,b1+exp(b2*x)) 
      converged = 1
      print(paste("Optimal intercept:", b1, "Optimal slope:", b2))
      return(c(b1,b2))
    }
  }
}

data <- read.csv('data_s06.csv')

X <- data$x
Y <- data$y

n <- length(X)

b <- gradient_descent(X, Y, 0.001, 0.001, n, 10000)


#estimating variance

residuals <- (Y - (b[1] + exp(b[2]*X)))
variance <- sum(residuals^2)/n
print(variance)