# Replace directory with path to the Data folder
setwd("C:/Users/jonas/Desktop/Class/ECS132/ECS132TermProject/Data")
#setwd("/Users/lloydretro2/Desktop/ECS132/TermProject/Data/")
load("EDFfair/AdultFinal.RData",verbose=T) 

age <- adult$age

#Actual Plot
#hist(age, probability=TRUE)
plot(density(age), col = "black", ylim = c(0,.033), xlab = "x", ylab = "y", main = "Title")

x <- 1:100 # Graphing ages 1-100

# Method of Moments
A <- mean(age)
S2 <- var(age)

L <- A / S2 #lamb
C <- L * A  #r 

curve(dgamma(x, C, L), 0, 100, add = TRUE, col = "red")

#Maximum Likelihood
nLL <- function(C, L)
{
  loglik <- sum(dgamma(age, C, L, log = TRUE))
  return(-loglik)
}

z <- stats4::mle(minuslog=nLL, start = list(C = 1, L = .1))
zC <- z@coef["C"]
zL <- z@coef["L"]
curve(dgamma(x, zC, zL), 0, 100, add = TRUE, col = "green")