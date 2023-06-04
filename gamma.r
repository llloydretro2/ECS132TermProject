# Replace directory with path to the Data folder
setwd("C:/Users/jonas/Desktop/Class/ECS132/ECS132TermProject/Data")
load("EDFfair/AdultFinal.RData",verbose=T) 

#Actual Plot
hist(adult$age, probability=TRUE)
plot(density(adult$age))

x <- 1:100 # Graphing ages 1-100

# Method of Moments
A <- mean(adult$age)
S2 <- var(adult$age)

L <- A / S2 #lamb
C <- L * A  #r 

plot(dgamma(x, C, L))
lines(density(adult$age)) # How to plot two lines on the same graph

#Maximum Likelihood
logbar <- mean(log(adult$age))
n <- length(adult$age)

nLL <- function(C, L)
{
  theta <- 1 / L
  # from wikipedia
  loglik <- n * (C - 1) * logbar - n * log(gamma(C)) - n * C * log(theta) - n * A / theta
  return(-loglik)
}

z <- stats4::mle(minuslog=nLL, start = list(C = 1, L = .1))
zC <- z@coef["C"]
zL <- z@coef["L"]
plot(dgamma(x, zC, zL))