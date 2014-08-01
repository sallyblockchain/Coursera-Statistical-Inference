## P-values
# The Earth is Round (p < .05)
# Compare what we calculated to our hypothetical distribution and see if
# the value is "extreme" (p-value)
pt(0.8, 15, lower.tail = F) # the probability of seeing evidence as extreme or 
# more extreme than that actually obtained under H0
choose(8, 7) * 0.5^8 + choose(8, 8) * 0.5^8
pbinom(6, size = 8, prob = .5, lower.tail = F) # >= 7
ppois(9, 5, lower.tail = F) # >= 10
pnorm(-0.355, lower.tail = F)

## Power
sigma <- 10
mu_0 <- 0
mu_a <- 2
n <- 100
alpha <- .05
plot(c(-3, 6), c(0, dnorm(0)), type = "n", frame = F, xlab = "Z value", ylab = "")
xvals <- seq(-3, 6, length = 1000)
lines(xvals, dnorm(xvals), type = "l", lwd = 3)
lines(xvals, dnorm(xvals, mean = sqrt(n)*(mu_a - mu_0) / sigma), lwd = 3)
abline(v = qnorm(1 - alpha))
# power.t.test -> T-test power
power.t.test(n = 16, delta = 2 / 4, sd = 1, type = "one.sample", alt = "one.sided")$power
power.t.test(n = 16, delta = 2, sd = 4, type = "one.sample", alt = "one.sided")$power
power.t.test(n = 16, delta = 100, sd = 200, type = "one.sample", alt = "one.sided")$power
power.t.test(power = .8, delta = 2 / 4, sd = 1, type = "one.sample", alt = "one.sided")$n
power.t.test(power = .8, delta = 2, sd = 4, type = "one.sample", alt = "one.sided")$n
power.t.test(power = .8, delta = 100, sd = 200, type = "one.sample", alt = "one.sided")$n

## Multiple testing
# Error measure, correction
# no true positives
set.seed(1010093)
pValues <- rep(NA, 1000)
for(i in 1:1000) {
  y <- rnorm(20)
  x <- rnorm(20)
  pValues[i] <- summary(lm(y ~ x))$coeff[2, 4]
}
sum(pValues < 0.05) # 51
# Controls FWER: Family wise error rate
sum(p.adjust(pValues, method = "bonferroni") < 0.05)
# Controls FDR: False discovery rate
sum(p.adjust(pValues, method = "BH") < 0.05)
# 50% true positives
set.seed(1010093)
pValues <- rep(NA, 1000)
for(i in 1:1000) {
  x <- rnorm(20)
  # first 500 beta = 0, last 500 beta = 2
  if (i <= 500) {
    y <- rnorm(20)
  } else {
    y <- rnorm(20, mean = 2*x)
  }
  pValues[i] <- summary(lm(y ~ x))$coeff[2, 4]
}
trueStatus <- rep(c("zero", "not zero"), each = 500)
table(pValues < 0.05, trueStatus)
# Controls FWER: Family wise error rate
table(p.adjust(pValues, method = "bonferroni") < 0.05, trueStatus)
# Controls FDR: False discovery rate
table(p.adjust(pValues, method = "BH") < 0.05, trueStatus)
par(mfrow = c(1, 2))
plot(pValues, p.adjust(pValues, method = "bonferroni"), pch = 19)
plot(pValues, p.adjust(pValues, method = "BH"), pch = 19)

## Resampled inference
# The jackknife: a tool for estimating standard errors and the bias of estimators
# Estimate the bias and standard error of the median
library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
theta <- median(x)
jk <- sapply(1 : n, function(i) median(x[-i]))
thetaBar <- mean(jk)
biasEst <- (n - 1)*(thetaBar - theta)
seEst <- sqrt((n - 1)*mean((jk - thetaBar) ^ 2))
c(biasEst, seEst) # 0.0000000 0.1014066
library(bootstrap)
temp <- jackknife(x, median)
c(temp$jack.bias, temp$jack.se) # 0.0000000 0.1014066

## Bootstrapping
B <- 1000
resamples <- matrix(sample(x, n*B, replace = T), B, n)
medians <- apply(resamples, 1, median)
sd(medians)
quantile(medians, c(.025, .975))
hist(medians)

## Permutation tests
data(InsectSprays)
boxplot(count ~ spray, data = InsectSprays)
subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"), ]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1 : 10000, function(i) testStat(y, sample(group)))
observedStat # 13.25
mean(permutations > observedStat) # 0
# p-value = 0 here
hist(permutations, breaks = 30
