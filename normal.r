# Replace directory with path to the Data folder
setwd("C:/Users/jonas/Desktop/Class/ECS132/ECS132TermProject/Data")
load("fairml/national.longitudinal.survey.rda", verbose=T)

# Save to the local variable
wt <- national.longitudinal.survey$weight

# Actual Plot
# hist(lawschoolbrief$GPA, probability=TRUE)
plot(density(wt), col = "black", xlab = "x", ylab = "y", main = "Title")

# Average
A <- mean(wt)
# Variance
S2 <- var(wt)

sd <- sqrt(S2)

curve(dnorm(x, A, sd), 0, 325, add = TRUE, col = "red", lty = "dashed")


nLL <- function(m, s)
{
  loglik <- sum(dnorm(wt, m, s, log = TRUE))
  return(-loglik)
}

z <- stats4::mle(minuslog=nLL, start = list(m = .1, s = .1))
zA <- z@coef["m"]
zSd <- z@coef["s"]
curve(dnorm(x, zA, zSd), 0, 325, add = TRUE, col = "green", lty = "dotted")

legend_labels <- c("Data", "MM", "MLE")
legend_colors <- c("black", "red", "green")
lengend_ltys  <- c("solid", "dashed", "dotted")
legend("topright", legend = legend_labels, col = legend_colors, lty = lengend_ltys)
