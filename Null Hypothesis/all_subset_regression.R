(load('Precipitation.RData'))

getmodels<-function(y,xnames,m=1:length(xnames)){
  #all possible combinations of the xvalues
  xvalues<-lapply(m,combn,x=xnames)
  
  #all possible xvalues combined for formula 
  xvalues.formula<-array(unlist(lapply(xvalues,apply,MARGIN=2,paste,collapse="+")))
  
  #constract formulas for all possible models
  paste(y,xvalues.formula,sep="~")
}

datanames <- names(Precipitation)

y <- datanames[1]
xnames <- datanames[2:length(datanames)]
#getting all the subsets of models
models <- getmodels(y,xnames)


Y <- as.numeric(unlist(Precipitation[1]))
y_mean <- mean(Y)
N <- length(models)

all_subset_regression <- function(models, y, y_mean, N, d){
  
adjusted_R <- 1:N;
rms_d <- 1:N
C_p <- 1:N
mallows_dist <- 1:N
#adjusted R
for(i in 1:N){
  
  fit <- lm(models[i], data = Precipitation)
  RSS <- sum((fit$residuals)^2)
  TSS <- sum((Y-y_mean)^2)
  d <- length(fit$coefficients)-1 
  adjusted_R[i] <- 1 - ((RSS*(1/(N-d)))/(TSS*(1/(N-1))))
  
}

#Residual Mean Square

for(i in 1:N){
  fit <- lm(models[i], data = Precipitation)
  RSS <- sum((fit$residuals)^2) 
  d <- length(fit$coefficients)-1 
  rms_d[i] <- RSS*(1/(N-d))
  
}


#Mallows Cp statistic

#RMS for largest model
fit <- lm(models[N], data = Precipitation)
RSS <- sum((fit$residuals)^2) 
d <- length(fit$coefficients)-1 
rms_max <- RSS*(1/(N-d))

for(i in 1:N){
  fit <- lm(models[i], data = Precipitation)
  RSS <- sum((fit$residuals)^2) 
  d <- length(fit$coefficients)-1 
  
  C_p[i] <- ((RSS)/(rms_max)-(N-2*d))
  
  mallows_dist[i] <- abs(C_p[i] - d)
}

index <- which.max(adjusted_R)
print("R square adjusted selected model:")
print( models[index])
index1 <- which.min(rms_d)
print("Residual mean squared selected model:")
print( models[index1])
index2 <- which.min(mallows_dist)
print("Mallows Cp selected model:")
print( models[index2])

}


#Backward Elimination
alpha <- 0.05

backward_elimination<- function(models,alpha){
 # Fit largest model 
k = length(xnames)
for (i in 1:k){

models <- getmodels(y, xnames)
n <- length(models)
fit <- lm(models[n], data = Precipitation)
curr_model <- fit
d <- length(fit$coefficients)-1 
f_quantile<-qf(1-alpha, df1=(1), df2=(N-d))
T_val <- summary(curr_model)[["coefficients"]][, "t value"]
ind <- which.min(T_val)
print(T_val)
print(paste("F quantile",f_quantile))
print(paste("index to be eliminated",(ind-1)))
if(T_val[ind] > f_quantile){
  return(summary(curr_model))
}
else{
  xnames <- xnames[-(ind-1)]
 
}
}
}


all_subset_regression(models,Y,y_mean,N,d)
backward_elimination(models,alpha)
