# Replace directory with path to the Data folder
#setwd("/Users/lloydretro2/Desktop/ECS132/TermProject/Data")
#setwd("C:/Users/jonas/Desktop/Class/ECS132/ECS132TermProject/Data")
#load("qeML/weatherTS.RData",verbose=T) 
load("weatherTS.RData",verbose=T)

# Save to the local variable
prec <- weatherTS$PRECTOT

# Default plots
#hist(prec, probability=TRUE, main = "Default Histogram of Precipitation", xlab = "Total Precipitaiton")
plot(density(prec), col = "black", xlab = "Total Precipitaiton", ylab = "Density", main = "Default Density of Precipitation")

# Modified Plot
#hist(prec, probability=TRUE, ylim = c(0,.55), breaks=40)
#plot(density(prec , adjust = 3), col = "black", xlab = "x", ylab = "y", main = "Title")

A <- mean(prec)
L <- 1 / A

S2 <- var(prec)
vL <- sqrt(1/S2)

curve(dexp(x, L), 0, 60, add = TRUE, col = "red", lty = "dashed")
curve(dexp(x, vL), 0, 60, add = TRUE, col = "blue")

nLL <- function(L)
{
  loglik <- sum(dexp(prec, L, log = TRUE))
  return(-loglik)
}

z <- stats4::mle(minuslog=nLL, start = list(L = .1))
zL <- z@coef["L"]
curve(dexp(x, zL), 0, 60, add = TRUE, col = "green", lty = "dotted")

legend_labels <- c("Data", "MM (mean)", "MM (var)", "MLE")
legend_colors <- c("black", "red", "blue", "green")
lengend_ltys  <- c("solid", "dashed", "solid", "dotted")
legend("topright", legend = legend_labels, col = legend_colors, lty = lengend_ltys)