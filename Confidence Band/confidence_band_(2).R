(load('steam_temperature.RData'))

Y <- t(as.data.frame(steam.temp[1]))
X <- t(as.data.frame(steam.temp[2]))

#convert temperatures to fahrenheit
X <- (X * 1.8)+ 32

new_x <- rep(0,12)
N <- length(Y)
# adding increase in Temperature by 5 degrees
for (i in 14:25){
  new_x[i-13] <- X[i] + 5
}
new_x <- as.vector(new_x)
Y<-t(Y)
X<-t(X)
linearfit <- lm(Y~X)
range_x <- 70:200
plot(X,Y)
lines(range_x, linearfit$coefficients[1]+linearfit$coefficients[2]*range_x, col="green")
sigma <- sqrt(sum((linearfit$residuals)^2))/length(Y)
#prediction
predicted_values <- rep(0,12)
for(i in 1:12){
pred_steam <- linearfit$coefficients[1]+linearfit$coefficients[2]*(new_x[[i]])
predicted_values[i] <- pred_steam
}
pdf(file='Task_5_3.pdf')
plot(new_x,predicted_values,pch=20)


#From 2.7
x_mean <- mean(new_x)
x_sq_m <- mean(new_x^2)
conf_band <- (qnorm(0.975))*(sigma/sqrt(N))* sqrt(1 + ((new_x - x_mean)^2/(x_sq_m - (x_mean)^2)))

#confidence intervals for each prediction
CI <- cbind(predicted_values-conf_band,predicted_values+conf_band)

points(new_x,CI[,1],pch=6)
points(new_x,CI[,2],pch=2)

#mean Estimated Steam
estimated_mean_steam <- mean(predicted_values)
est_Y <- rep(estimated_mean_steam,12)
lines(new_x,est_Y, type="l", lty=2)

#Confidence band of predicted steam for 3rd year
lines(new_x,CI[,1],col="blue",lty=2,lwd=2)
lines(new_x,CI[,2],col="blue",lty=2,lwd=2)

dev.off()

