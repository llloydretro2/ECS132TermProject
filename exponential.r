# Replace directory with path to the Data folder
setwd("C:/Users/jonas/Desktop/Class/ECS132/ECS132TermProject/Data")
load("qeML/weatherTS.RData",verbose=T) 


# Save to the local variable
prec <- weatherTS$PRECTOT

# Actual Plot
hist(prec, probability=TRUE, ylim = c(0,.55), breaks = 40)
#plot(density(prec), col = "black", xlab = "x", ylab = "y", main = "Title")


# Average # 1/l
A <- mean(prec)
# lambda
lambda <- 1 / A

curve(dexp(x, lambda), 0, 60, add = TRUE, col = "red")

nLL <- function(lambda)
{
  loglik <- sum(dexp(prec, rate = lambda, log = TRUE))
  return(-loglik)
}

z <- stats4::mle(minuslog=nLL, start = list(lambda = .1))
zLambda <- z@coef["lambda"]
curve(dexp(x, zLambda), 0, 60, add = TRUE, col = "green")

legend_labels <- c("Data", "MM", "MLE")
legend_colors <- c("black", "red", "green")
legend("topright", legend = legend_labels, col = legend_colors, lty = 1)