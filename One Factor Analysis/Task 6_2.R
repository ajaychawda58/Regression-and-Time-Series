mydata <- read.delim('mygardens.RData')

Y <- mydata$ozone
X <- mydata$garden

X_range <- 1:length(X)

plot(X_range,Y,xlab = 'Index',ylab = 'Ozone', main = 'ANOVA 1')
y_mean <- mean(Y)

lines(X_range, rep(y_mean,length(Y)))
segments(X_range,Y,x1=X_range,y1=y_mean)


#separate Data for A and B
ind_A <- Y[which(X == "A")]
mean_A <- mean(ind_A)
ind_B <- Y[which(X == "B")]
mean_B <- mean(ind_B)

#plot for different groups
X_rangeS <- 1:length(ind_A)
plot(X_rangeS,ind_A, col = 'red', ylim=c(0,10),xlab = 'Index',ylab = 'Ozone',, main = 'ANOVA 2')
points(ind_B, col='blue')
lines(X_rangeS, rep(mean_A,length(ind_A)),col='red')
lines(X_rangeS, rep(mean_B,length(ind_B)),col='blue')
segments(X_rangeS,ind_A,x1=X_rangeS,y1=mean_A,col='red')
segments(X_rangeS,ind_B,x1=X_rangeS,y1=mean_B,col='blue')

#Sum of Squares
#Total
D_g <- sum((Y-y_mean)^2) 
#group A and B
D_b_A <- ((mean_A-y_mean)^2)
D_b_B <- ((mean_B-y_mean)^2)
D_b <- length(ind_A)*D_b_A + length(ind_B)*D_b_B

D_I <- sum((ind_A-mean_A)^2) + sum((ind_B-mean_B)^2)

#F Test Statistic
R = ((length(Y)-2)/(2-1))* (D_g - D_I)/D_I
print(paste('F- value = ', R))
#ANOVA command
print("ANOVA Table")
anova(lm(Y~X))
