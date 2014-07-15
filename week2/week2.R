## iid Bernoulli trails
# Plotting all possible likelihoods for a small n
n <- 5
pvals <- seq(0, 1, length = 1000)
plot(c(0, 1), c(0, 1.2), type = "n", frame=F, xlab = "p",
     ylab = "likelihood")
text((0 : n) /n, 1.1, as.character(0 : n))
sapply(0 : n, function(x) {
    phat <- x / n
    if (x == 0) lines(pvals, ((1 - pvals) / (1 - phat))^(n - x), 
                      lwd = 3)
    else if (x == n) lines(pvals, (pvals / phat)^x, 
                           lwd = 3)
    else lines(pvals, (pvals / phat)^x*((1-pvals)/(1-phat))^(n-x), 
               lwd = 3)
})
title(paste("Likelihoods for n = ", n))
# Binomial distribution
choose(8, 7)*0.5^8 + choose(8, 8)*0.5^8
pbinom(6, size = 8, prob = 0.5, lower.tail = F)
plot(pvals, dbinom(7, 8, pvals) / dbinom(7, 8, 7/8), 
     lwd = 3, frame = F, type = "l", xlab = "p", ylab = "likelihood")
# Normal distribution
zvals <- seq(-3, 3, length = 1000)
plot(zvals, dnorm(zvals), type = "l", lwd = 3,
     frame = F, xlab = "z", ylab = "Density")
sapply(-3 : 3, function(k) abline(v = k))
# qnorm(0.95, mean = mu, sd = sd)
# Poisson distribution:
# Modeling event/time data, radioactive decay, survival data, unbounded count data,
#           contingency tables; Approximating binomials when n is large
#           and p is small
# 2.5 people / hour, 4 hours, prob of <=3 show up for the whole time
ppois(3, lambda = 2.5 * 4)
# Poisson -> binomial distribution
# p = 0.01, 500 times, prob of <=2 successes
pbinom(2, size = 500, prob = 0.01) #  0.1233858
ppois(2, lambda = 500 * 0.01) # 0.124652
## Law of large numbers
n <- 10000
means <- cumsum(rnorm(n)) / (1 : n)
plot(1 : n, means, type = "l", lwd = 2, 
     frame = F, ylab = "cumulative means", xlab = "sample size")
abline(h = 0)
## Confidence interval
library(UsingR)
data(father.son)
x <- father.son$sheight
(mean(x) + c(-1, 1)*qnorm(.975)*sd(x) / sqrt(length(x))) / 12
# Poisson interval
x <- 5
t <- 94.32
lambda <- x / t
round(lambda + c(-1, 1)*qnorm(0.975)*sqrt(lambda / t), 3)
poisson.test(x, T = 94.32)$conf
exp(confint(glm(x ~ 1 + offset(log(t)), family = poisson(link = log))))
## T interval
# sleep data
data(sleep)
head(sleep)
g1 <- sleep$extra[1:10]
g2 <- sleep$extra[11:20]
difference <- g2 - g1
mn <- mean(difference)
s <- sd(difference)
n <- 10
mn + c(-1, 1) * qt(0.975, n - 1)*s / sqrt(n)
t.test(difference)$conf.int
