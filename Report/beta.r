# Replace directory with path to the Data folder
#setwd("C:/Users/jonas/Desktop/Class/ECS132/ECS132TermProject/Data")
#load("EDFfair/lawschoolbrief.RData",verbose=T)
load("lawschoolbrief.RData",verbose=T)

# Save to the local variable
gpa <- lawschoolbrief$GPA

fac <- 4.3

# Actual Plot
#hist(gpa, probability=TRUE, xlab = "GPA", main = "Histogram of GPA")
plot(density(gpa), col = "black", xlab = "GPA", ylab = "Density", main = "Density of GPA w/ Approximation (0, 4.3)", xlim = c(0,fac))

# Average
A <- mean(gpa) / fac
# Variance
S2 <- var(gpa) / fac^2
# alpha
alpha <- A * ((A * (1 - A) / S2) - 1)
# beta
beta <- alpha / A - alpha

curve(dbeta(x/fac, alpha, beta)/fac, 0, fac, add = TRUE, col = "red")


nLL <- function(alpha, beta)
{
  loglik <-sum(dbeta(gpa/fac, shape1 = alpha, shape2 = beta, log = TRUE))
  return(-loglik)
}

z <- stats4::mle(minuslog=nLL, start = list(alpha = .1, beta = .1))
zAlpha <- z@coef["alpha"]
zBeta <- z@coef["beta"]
curve(dbeta(x/fac, zAlpha, zBeta) / fac, 0, fac, add = TRUE, col = "green")

legend_labels <- c("Data", "MM", "MLE")
legend_colors <- c("black", "red", "green")
legend("topright", legend = legend_labels, col = legend_colors, lty = 1, x = "topleft")
