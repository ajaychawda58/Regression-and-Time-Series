data <- read.csv('data_sheet05.csv')

#Fitting the model
fmla <- as.formula("data$Y ~ data$X + log(data$X)")
fit <- lm(fmla)

#predict x = 4.5

x <- 4.5
pred <- fit$coefficients[1] + fit$coefficients[2]*x + fit$coefficients[3]*log(x)

#confidence interval
sigma <- sqrt(sum((fit$residuals)^2))/length(data$X)
N <- length(data$X)
x_mean <- mean(data$X)
x_sq_m <- mean((data$X)^2)
D <- (qnorm(0.975))*(sigma/sqrt(N))* sqrt(1 + ((x - x_mean)^2/(x_sq_m - (x_mean)^2)))
CI <- c(pred-D, pred+D)
range_x <- seq(from=1,to=5,by=0.1)
#Confidence Band for regression function
conf_band <- sqrt(qf((0.975),2,N-2))*(sigma*sqrt(2/N))* sqrt(1 + ((x - x_mean)^2/(x_sq_m - (x_mean)^2)))
pred_reg <- fit$coefficients[1] + fit$coefficients[2]*range_x + fit$coefficients[3]*log(range_x)
reg_CI <- cbind(pred_reg - conf_band,pred_reg + conf_band )

#Plot data and regression line
plot(data$X,data$Y)

lines(range_x,fit$coefficients[1] + fit$coefficients[2]*range_x + fit$coefficients[3]*log(range_x))
#predicted value
points(x,pred,pch=20)
#CI for prediction
points(x,CI[1],col="red",pch=20 )
points(x,CI[2],col="blue",pch=20)
#Confidence band
lines(range_x,reg_CI[,1],col="blue",lty=2,lwd=2)
lines(range_x,reg_CI[,2],col="blue",lty=2,lwd=2)

