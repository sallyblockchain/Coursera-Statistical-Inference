### Statistical inference 
## Definition: 
# Statistical Inference: the process fo drawing formal conclusions from data. 
# infer facts about a population using noisy statistical data 
# where uncertainty must be accounted for.
# Goals: 
# 1. Estimate and quantify the uncertainty of an estimate of a 
#    population quantity
# 2. Determine whether a population quantity is a benchmark value
# 3. Infer a mechanistic relationship when quantities are measured with noise
# 4. Dtermine the impact of a policy
# Tools: 
# 1. randomization - balancing unobserved variables
# 2. random sampling - representative of the population
# 3. sampling models - iid
# 4. hypothesis testing - decision making when uncertain
# 5. confidence intervals - quantifyting uncertaninty in estimation
# 6. probability models - data <-> population of interest
# 7. study design - designing an experiment to minimize biases and variability
# 8. nonparametric bootstrppping 
# 9.permutation, randomization and exchangeability testing
# Styles of inference:
# 1. Frequency probability
# 2. Frequency inference
# 3. Bayesian probability: the probability calculus of beliefs, 
#                          given that beliefs follow certain rules
# 4. Bayesian inference

## Probabaility
x <- c(-0.5, 0, 1, 1, 1.5)
y <- c(0, 0, 2, 0, 0)
plot(x, y, lwd = 3, frame = FALSE, type = "l")
par("mar")
par(mar = c(6, 6, 3, 2)) # B,L,U,R
1.5 * 0.75 / 2
pbeta(0.75, 2, 1) # 0.5625
pbeta(c(0.4, 0.5, 0.6), 2, 1) # 0.16 0.25 0.36
# Quantile
sqrt(0.5)
qbeta(0.5, 2, 1) # 0.7071068
# Sample estimates Population.
# estimand, estimator(sample median)
## 
# The center of mass is the empirical mean
library(HistData)
library(UsingR)

meanChild <- mean(galton$child)
lines(rep(meanChild, 100), seq(0, 150, length = 100), 
      col = "red", lwd = 5)
library(manipulate)
myHist <- function(mu) {
    hist(galton$child, col = "blue", breaks = 100)
    lines(c(mu, mu), c(0, 150), col = "red", lwd = 5)
    mse <- mean((galton$child - mu)^2)
    text(63, 150, paste("mu = ", mu))
    text(63, 140, paste("Imbalance = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))
##
# The expected value of the sample mean is the population mean that
# it's trying to estimate.
# When the expected value of an estimator is what its trying to 
# estimate, we say that the estimator is unbiased.
data(father.son)
x <- father.son$sheight
n <- length(x)
round(c(sum((x - mean(x))^2) / (n-1), var(x), var(x)/n, 
        sd(x), sd(x) / sqrt(n)), 2)
# 7.92 7.92 0.01 2.81 0.09
## Chi-square
# CI for the standard deviation of sons' heights from Galton's data 
x <- father.son$sheight
s <- sd(x)
n <- length(x)
round(sqrt((n - 1) * s ^ 2/qchisq(c(0.975, 0.025), n - 1)), 3)
