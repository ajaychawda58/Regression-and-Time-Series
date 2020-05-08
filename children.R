 library(ggplot2)

#reading data from csv file
data <- read.csv('children.csv')
#Y = age, x1 = weight, x2 = height
Y <- data$age
x1 <- data$weight
x2 <- data$height

#Fitting Linear Models 
a.fit <- lm(Y~x1,data, model=TRUE)
a.fit$coefficients

b.fit <- lm(Y~x2,data,model=TRUE)
b.fit$coefficients

c.fit <- lm(Y~x1+x2)
c.fit$coefficients

#Plot of model 1 = a.fit
w <- 40:80
b1a <- a.fit$coefficients[1]
b2a <- a.fit$coefficients[2]
plot(x1,Y, col='blue',xlab='weight(kg)',ylab='Age(Years')
lines(w, (b1a+b2a*w),col = "red", lwd =2 )

#calculate and plot 0.95 confidence region for model A
new.x1 = seq(min(x1), max(x1), length.out=12)
conf_interval <- predict(a.fit, newdata = data.frame(x=new.x1), 
                         interval="confidence",
                          level = 0.95)
lines(sort(x1), fitted(a.fit)[order(x1)], col='red', lwd=3) 
polygon(c(rev(new.x1), new.x1), 
        c(rev(conf_interval[ ,3]), conf_interval[ ,2]), 
        density=10, col = 'blue', border = NA)

lines(new.x1, conf_interval[ ,3], lty = 'dashed', col = 'red')
lines(new.x1, conf_interval[ ,2], lty = 'dashed', col = 'red')


df.a <- data.frame(x = new.x1,
                 F =conf_interval[,1],
                 L =conf_interval[,2],
                 U =conf_interval[,3])

ggplot(df.a, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L))

#Plot of model 2 = b.fit
h <- 40:80
b1b <- b.fit$coefficients[1]
b3b <- b.fit$coefficients[2]
plot(x2,Y, col='blue',xlab='height (cm)',ylab='Age(Years')
lines(h, (b1b+b3b*h),col = "green", lwd =2 )

#calculate and plot 0.95 confidence region for model B
new.x2 = seq(min(x2), max(x2), length.out=12)
conf_interval <- predict(b.fit, newdata = data.frame(x=new.x2), 
                         interval="confidence",
                         level = 0.95)
lines(sort(x2), fitted(b.fit)[order(x2)], col='green', lwd=3) 
polygon(c(rev(new.x2), new.x2), 
        c(rev(conf_interval[ ,3]), conf_interval[ ,2]), 
        density=10, col = 'blue', border = NA)

lines(new.x2, conf_interval[ ,3], lty = 'dashed', col = 'red')
lines(new.x2, conf_interval[ ,2], lty = 'dashed', col = 'red')


df.b <- data.frame(x = new.x2,
                   F =conf_interval[,1],
                   L =conf_interval[,2],
                   U =conf_interval[,3])

ggplot(df.a, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L))


#Plot of model 3 = c.fit
b1c <- c.fit$coefficients[1]
b2c <- c.fit$coefficients[2]
b3c <- c.fit$coefficients[3]
plot(x2,Y, col='blue',xlab='height (cm)',ylab='Age(Years')
lines(h, (b1c+b2c*w+b3c*h),col = "blue", lwd =2 )
plot(x1,Y, col='blue',xlab='weight(kg)',ylab='Age(Years')
lines(w, (b1c+b2c*w+b3c*h),col = "blue", lwd =2 )

#Residuals - The error between the prediction of the model 
#and the actual results.
#Smaller residuals are better.
res_a <- sum(a.fit$residuals)
res_b <- sum(b.fit$residuals)
res_c <- sum(c.fit$residuals)
res <- data.frame(r=c(res_a, res_b, res_c), 
                  rn = c('res_a','res_b','res_c'))
res$rn[which.min(res$r)]


summary(a.fit)
summary(b.fit)
summary(c.fit)
