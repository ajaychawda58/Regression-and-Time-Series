mydata <- read.delim('fertilizer_seed.txt')

global_mean <- mean(mydata$number.of.plants)

factor1_a <- mean(mydata$number.of.plants[which(mydata$Fertilizer == 'a')])
factor1_b <- mean(mydata$number.of.plants[which(mydata$Fertilizer == 'b')])
factor1_c <- mean(mydata$number.of.plants[which(mydata$Fertilizer == 'c')])
factor1_d <- mean(mydata$number.of.plants[which(mydata$Fertilizer == 'd')])
factor1_e <- mean(mydata$number.of.plants[which(mydata$Fertilizer == 'e')])

seed2_a <- mean(mydata$number.of.plants[which(mydata$Seed == 'A-402')])
seed2_b <- mean(mydata$number.of.plants[which(mydata$Seed == 'B-894')])
seed2_c <- mean(mydata$number.of.plants[which(mydata$Seed == 'C-952')])

p = 5
q = 3
m = 2
n <- length(mydata$Observation)
#Sum of Squares

D_1 <- (m*q)*((factor1_a-global_mean)^2 + (factor1_b-global_mean)^2 +
                (factor1_c-global_mean)^2 + (factor1_d-global_mean)^2 +
                  (factor1_e-global_mean)^2 )

D_2 <- (m*p)*((seed2_a - global_mean)^2 + (seed2_b - global_mean)^2 +
                (seed2_c - global_mean)^2 )

 
D_R <- 0

for(i in 1:n){
  if(mydata$Seed[i] == 'A-402'){
    seed_mean <- seed2_a
  }
  else if(mydata$Seed[i] == 'B-894'){
    seed_mean <- seed2_b
  }
  else{
    seed_mean <- seed2_c
  }
  if(mydata$Fertilizer[i] == 'a'){
    factor_mean <- factor1_a
  }
  else if(mydata$Fertilizer[i] == 'b'){
    factor_mean <- factor1_b
  }
  else if(mydata$Fertilizer[i] == 'c'){
    factor_mean <- factor1_c
  }
  else if(mydata$Fertilizer[i] == 'd'){
    factor_mean <- factor1_d
  }
  else{
    factor_mean <- factor1_e
  }
  
  D_r <- (mydata$number.of.plants[i] - seed_mean - factor_mean + global_mean)^2
  D_R <- (D_R + D_r)
}

#Mean sum of Squares

q1 <- (D_1/(p-1))
q2 <- (D_2/(q-1))
qr <- (D_R/(n-p-q+1))

#F-value
R1 <- q1/qr
R2 <- q2/qr

#interaction between 1 and 2
D_12 <- 0
k = 1
while(k<n){
  mean <- mean(c(mydata$number.of.plants[k],mydata$number.of.plants[k+1]))
  D_12 <- D_12 + (mydata$number.of.plants[k]- mean)^2 + (mydata$number.of.plants[k+1]-mean)^2
  k <- k + 2
}

R <- ((n-(p*q))*(D_R-D_12))/(((p-1)*(q-1))*D_12)

print(R1)
print(R2)
print(R)
#a)
if(R1 > (qf(0.95,(p-1),(n-p-q+1)))){
  print(" The number of plants for each seed type is not homogeneous and we reject the null hypothesis")
}else{
  print(" The number of plants for each seed type is homogeneous and we accept the null hypothesis")
}

#" The number of plants for each seed type is not homogeneous and we reject the null hypothesis"

#b)

if(R2 > (qf(0.95,(q-1),(n-p-q+1)))){
  print(" There is significance difference between seed types and we reject the null hypothesis")
}else{
  print(" There is no significance difference between seed types and we accept the null hypothesis")
}

#" There is significance difference between seed types and we reject the null hypothesis"

#c)
if(R > (qf(0.95,((p-1)*(q-1)),(n-(p*q))))){
  print(" The assumption of no interaction is wrong and we reject the null hypothesis")
}else{
  print(" The assumption of no interaction is correct and we accept the null hypothesis")
}

#" The assumption of no interaction is correct and we accept the null hypothesis"
