## Quiz 4.
# Problem 1.
alpha <- .05
mu <- 12
sd <- 4
n <- 100
z <- qnorm(alpha)
m0 <- mu + z * sd / sqrt(n)
m0 # 11.34206

# Problem 2. T-test
pharm <- data.frame(baseline = c(140, 138, 150, 148, 135), 
                    week2 = c(132, 135, 151, 146, 130))
t.test(pharm$baseline, pharm$week2, alternative = "two.sided", paired = T) # p-value = 0.08652

# problem 3.CI
n <- 9
mu <- 1100
sd <- 30
alpha <- .05
tstat <- qt(1 - alpha/2, n - 1)
mu + c(-1, 1)*tstat*sd / sqrt(n) # 1076.94 1123.06

# Problem 4. P-value
library(stats)
binom.test(x = 3, n = 4, p = .5, alt = "greater") # p-value = 0.3125

# Problem 5.
p <- 1 / 100
pr <- 10 / 1787
n <- 1787
serror <- sqrt(p * (1-p) / n)
z <- (p-pr) / serror
pnorm(z, lower.tail = F) # 0.03066625

# Problem 6.Two-sided T-test
n1 <- 9
n2 <- 9
df <- n1 + n2 - 2
meanTreat <- -3
meanPlacebo <- 1
sdTreat <- 1.5
sdPlacebo <- 1.8
pooledVar <- (sdTreat^2 * n1 + sdPlacebo^2 * n2)/df
se.diff <- sqrt(pooledVar/n1 + pooledVar/n2)
tstat <- (meanTreat - meanPlacebo) / se.diff
tstat
pValue <- 2 * pt(tstat, df = df)
pValue # 0.0001852248

# Problem 7.
# No calculation is needed. We wouldn't reject because the 95% CI contains
# the 90% CI. µ = 1078 falls within the interval, so we don't reject H0.

# Problem 8. Power
n <- 100
mu <- .01
sd <- .04
power.t.test(n, delta = mu, sd = sd, type = "one.sample", alt = "one.sided")$power # 0.7989855

# Problem 9. Power
power <- .9
power.t.test(power = power, delta = mu, sd = sd, type = "one.sample", alt = "one.sided")$n # 138.3856

# Problem 10.
# As you increase the type one error rate, α, or use one-sided test instead of two-sided test,
# or increase n, power will get larger.

# Problem 11. P-value of two-sided Z-test
n <- 288
mu1 <- 44
mu2 <- 42.04
sd <- 12
se <- sd * sqrt(1/n + 1/n)
z <- (mu1 - mu2)/se
z
pValue <- 2 * pnorm(-abs(z))
pValue # 0.04999579

# Problem 12. FWER: family wise error rate
m <- 10
alpha <- .05
alphaFwer <- alpha / m
alphaFwer
