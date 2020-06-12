data <- read.delim('TUK_data.csv', sep=';')

#a)
#linear regression model
linear_fit <- lm(data$TUK_win_num~data$TUK_score)
plot(data$TUK_score,data$TUK_win_num, ylim=c(-1,2))

x <- 15:50
lines(x, linear_fit$coefficients[1]+linear_fit$coefficients[2]*x)

#This is not a good choice because the estimated variable is bounded and has an upper limit.
#Linear regression is a good choice for estimating unbounded values.
#here the output is binary and we need a different model than linear regression.

#logistic regression model
log_reg <- glm(data$TUK_win_num~data$TUK_score, family= 'binomial')

plot(data$TUK_score, log_reg$fitted.values)

#estimating function
#Our function calculates the means between winning scores
# and losing scores.then the mean of calculated means is used as threshold
# scores greater than threshold are estimated to have probability 1 of winning.
# we did not use binomial distribution since it would be similar to logistic regression 
# done earlier.
estimate <- function(data,score){
  mean_win <- mean(data$TUK_score[which(data$TUK_win_num == '1')])
  mean_loss <- mean(data$TUK_score[which(data$TUK_win_num == '0')])
  mean <- (mean_win + mean_loss)/2
  if(score > mean){
    victory_probability <- 1
  }
  else{
    victory_probability <- 0
  }
  return(victory_probability)
}


# Score= 26
score <- 26
prob <- estimate(data,score)
#point plotted with blue color
points(score,prob, col='blue')
#predicted with logistic regression
pred <- 1 /(1 + exp(-(log_reg$coefficients[1]+log_reg$coefficients[2]*score)))
points(score,pred, col='red' )
#comparing our function with glm()
segments(score,prob, x1=score,y1=pred)
