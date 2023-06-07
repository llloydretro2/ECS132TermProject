# Replace directory with path to the Data folder
setwd("/Users/lloydretro2/Desktop/ECS132/TermProject/Data/")
load("EDFfair/lawschoolbrief.RData",verbose=T)

# Actual Plot
# hist(lawschoolbrief$GPA, probability=TRUE)
plot(density(lawschoolbrief$GPA))

# Save to the local variable
gpa <- lawschoolbrief$GPA

# Average
A <- mean(gpa) / 5
# Variance
S2 <- var(gpa) / 25
# alpha
alpha <- A * ((A * (1 - A) / S2) - 1)
# beta
beta <- alpha / A - alpha

curve(dbeta(x/5, alpha, beta)/5, 0, 5, add = TRUE, col = 2)


nLL <- function(alpha, beta)
{
    # loglik <- (alpha - 1) * logbar + (beta - 1) * logbarp - N * log(beta(alpha, beta))
    loglik <-sum(dbeta(gpa/5, shape1 = alpha, shape2 = beta, log = TRUE))
    return(-loglik)
}

z <- stats4::mle(minuslog=nLL, start = list(alpha = .1, beta = .1))
zAlpha <- z@coef["alpha"]
zBeta <- z@coef["beta"]
curve(dbeta(x/5, zAlpha, zBeta) / 5, 0, 5, add = TRUE, col = 3)