library(zoo)
#load data
data(co2)
init_data <- data.frame(date=as.Date(as.yearmon(time(co2))), 
                        co2=as.matrix(co2))

#plot initial data
plot(init_data, ylim=c(310,370), type="l")

#erase values for 1964-03-01 (March) and 1964-04-01 (April)
modified_data <-init_data
modified_data[63,2] = NA
modified_data[64,2] = NA

#plot data with erased values
plot(modified_data, ylim=c(310,370), type="l")

#perform LSE for data with erased values
reg1 <- lm(modified_data$co2~modified_data$date)

reg1
reg1$coefficients
reg1$fitted.values
reg1$residuals

summary(reg1)

#plot regression line for data with erased values
t = modified_data$date[-63:-64]
plot(modified_data, ylim=c(310,370), type="l")
lines(t, reg1$fitted.values, col="red", lwd=2)

#add values back by interpolation
interpolated_data <- modified_data
interpolated_data$co2 <- na.approx(interpolated_data$co2)
plot(interpolated_data, ylim=c(310,370), type="l")

#perform LSE for data with interpolated values
reg2 <- lm(interpolated_data$co2~interpolated_data$date)

reg2
reg2$coefficients
reg2$fitted.values
reg2$residuals

summary(reg2)

#plot regression line for data with erased values
# and mark the corresponding values of regression line
plot(interpolated_data, ylim=c(310,370), type="l")
lines(interpolated_data$date, 
      reg2$fitted.values, col="red", lwd=2)
points(interpolated_data$date[63:64], 
       reg2$fitted.values[63:64], col="blue", lwd=3)

