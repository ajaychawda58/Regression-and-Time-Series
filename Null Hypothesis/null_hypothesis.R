
  #null hypothesis function
  null_hypothesis <- function(data,d,q,alpha){
    #length of vector
  N <- length(data$data)
  
  #Fitting polynomial with d degree
  param_d <- lm(data$data~poly(data$time,d), model =TRUE)
  #Fitting polynomial with q degree
  param_q <- lm(data$data~poly(data$time,q), model =TRUE)
  
  #From 2.5.2 (Construction of a test)
  D0_hat <- sum((param_q$residuals)^2)
  D_hat <- sum((param_d$residuals)^2)

  R <- (N - d)*(D0_hat - D_hat)/ (d-q)*D_hat
  f_quantile<-qf(1-alpha, df1=(d-q), df2=(N-d))

  if(R > f_quantile){
    return(TRUE)
  }
   return(FALSE)
  }
  

  data <- read.csv('data_sheet04.csv')
  d = 4
  q = 2
  alpha = 0.5
  null_hypothesis(data,d,q,alpha)