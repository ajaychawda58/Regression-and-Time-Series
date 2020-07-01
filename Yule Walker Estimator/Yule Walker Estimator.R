data <- read.table('data.csv', header = TRUE)

#ts.plot(data)
#X_mean <- lapply(data, mean)
YuleWalker_estimator <- function(data){
  #r0,r1,r2,r3
  r <- rep(1,4)
  r <- acf(data, type='covariance',plot=F)$acf[1:4]
  r_hat <- r[2:4]
  #R matrix with r0, r1 and r2 values
  R <- matrix(1,3,3)
  R[1,1] <- r[1]
  R[2,2] <- r[1]
  R[3,3] <- r[1]
  R[1,2] <- r[2]
  R[1,3] <- r[3]
  R[2,1] <- r[2]
  R[3,1] <- r[3]
  R[2,3] <- r[2]
  R[3,2] <- r[2]
  
  #From 6.4.13
  alpha_hat <- solve(R)%*%r_hat
  
  var_hat <- r[1] - (t(alpha_hat)%*%(r_hat))
  #print(R)
  #print(r)
  print('Estimated alpha')
  print(alpha_hat)
  print('Estimated Variance')
  print(var_hat)
  
  
}

YuleWalker_estimator(data)
