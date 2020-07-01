mydata <- load('Kreatinin.RData')



FPE <- function(data,p){
  n <- length(data)
  r <- rep(1,(p+1))
  r <- acf(data, type='covariance',plot=F)$acf[1:(p+1)]
  r_hat <- (r[2:(p+1)])
  
  #Toeplitz matrix
  R <- matrix(1,p,p)
  d <- row(R) - col(R)
  for(i in 1: (p-1)){
    R[which(d %in% i)] <- r[i+1]
    R[which(d %in% -i)] <- r[i+1]
    diag(R) <- r[1]
  }
  
  alpha_hat <- solve(R)%*%r_hat
  var_hat <- r[1] - (t(alpha_hat)%*%(r_hat))
  fpe <- (n+p)*var_hat/(n-p)
  return(fpe)
  
}

AIC <- function(data,p){
  n <- length(data)
  r <- rep(1,(p+1))
  r <- acf(data, type='covariance',plot=F)$acf[1:(p+1)]
  r_hat <- (r[2:(p+1)])
  
  R <- matrix(1,p,p)
  d <- row(R) - col(R)
  for(i in 1: (p-1)){
    R[which(d %in% i)] <- r[i+1]
    R[which(d %in% -i)] <- r[i+1]
    diag(R) <- r[1]
  }
  
  alpha_hat <- solve(R)%*%r_hat
  var_hat <- r[1] - (t(alpha_hat)%*%(r_hat))
  
  aic <- (n * log(var_hat)) + (2*p)
  return(aic)
}

BIC <- function(data,p){
  n <- length(data)
  r <- rep(1,(p+1))
  r <- acf(data, type='covariance',plot=F)$acf[1:(p+1)]
  r_hat <- (r[2:(p+1)])
  
  R <- matrix(1,p,p)
  d <- row(R) - col(R)
  for(i in 1: (p-1)){
    R[which(d %in% i)] <- r[i+1]
    R[which(d %in% -i)] <- r[i+1]
    diag(R) <- r[1]
  }
  
  alpha_hat <- solve(R)%*%r_hat
  var_hat <- r[1] - (t(alpha_hat)%*%(r_hat))
  
  bic <- (n * log(var_hat)) + (p*log(n))
  return(bic)
}



result_fpe <- rep(0,30)
result_aic <- rep(0,30)
result_bic <- rep(0,30)
for(p in 1:30){
  result_fpe[p] <- FPE(x,p)
  result_aic[p] <- AIC(x,p)
  result_bic[p] <- BIC(x,p)
  }
print(which.min(result_fpe))
print(which.min(result_aic))
print(which.min(result_bic))