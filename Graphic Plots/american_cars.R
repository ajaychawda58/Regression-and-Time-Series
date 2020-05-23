
data <- read.csv('american_cars.csv')

#plot mpg vs capacity

plot(x=data$capacity,y=data$mpg, xlab='capacity', ylab='MPG')

#Least square estimates and variance of residuals

Y <- data$mpg
X <- data$capacity
y_n = mean(Y)
x_n = mean(X)

b2 = sum((Y - y_n)*(X-x_n))/sum((X-x_n)^2)
b1 = (y_n - (b2*x_n))

#From theorem 2.2.3
sigma_squared <- (1/length(Y))*sum((Y - b2*X - b1)^2)
#From proposition 2.3.1
N <- length(Y)
residual_variance <- sigma_squared*(1-(1/N)-(1/N)*((X-x_n)^2)/(mean(X^2)-(x_n)^2))

#standard deviation of slope

#sse = sum of squared residuals
sse = 1/(length(Y)-2)*sum((Y - b2*X - b1)^2)
sse = sqrt(sse)
#ess = sum of squares of independent variable
ess = sqrt(sum((X-x_n)^2))

sd_slope = sse/ess

#checking with lm function
car_fit <- lm(Y~X,data=data, model=TRUE)
summary(car_fit)

#graphic plot
library(ggplot2)
data$residuals <- (Y-(b1+b2*X))
data$predicted <- (b1+b2*X)
pdf(file='Task 3_2.pdf')
x1 <- (0:350)
data$residuals <- (Y-(b1+b2*X))
par(mfrow = c (3,1))
#scatter plot
plot(x=(data$capacity),y=data$mpg, xlab='MPG', ylab='Capacity',main='Scatter Plot',pch=1)
#regression Line
plot(x=(data$capacity),y=data$mpg, xlab='MPG', ylab='Capacity',main='Regression Line')
lines(x1, b1+b2*x1,col='red',lwd=2)
#residuals
plot(data$residuals, col = 'blue',main='Residuals', pch=2)
dev.off()

#graphic using ggplot

ggplot(data, aes(x = capacity, y = mpg,group = capacity)) +  geom_point() + 
  geom_point(aes(y = residuals), shape = 1) + 
  geom_segment(aes(xend= capacity,yend= residuals))+
  geom_abline(slope = b2, intercept = b1,col='red')


  

  



