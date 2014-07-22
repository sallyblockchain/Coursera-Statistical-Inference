## Likelihood
# Assume the data arises from a family of distributions indexed by a parameter
# that represents a useful summary of the data.
# Use the likelihood to perform inference regarding the unknown parameter.
# Definition: given a statistical probability mass function or density, f(x, theta), 
# where theta is an unknown parameter, the likelihood is f viewed as a function of 
# theta for a fixed, observed value of x.

# plot likelihood
pvals <- seq(0, 1, length = 1000)
plot(pvals, dbinom(3, 4, pvals) / dbinom(3, 4, 3/4), type = "l", frame = F, 
     lwd = 3, xlab = "p", ylab = "likelihood / max likelihood")

lambda <- seq(0, .2, length = 1000)
likelihood <- dpois(5, 94 * lambda) / dpois(5, 5)
plot(lambda, likelihood, frame = F, lwd = 3, type = "l", xlab = expression(lambda))
lines(rep(5/94, 2), 0 : 1, col = "red", lwd = 3)
lines(range(lambda[likelihood > 1 / 16]), rep(1 / 16, 2), lwd = 2)
lines(range(lambda[likelihood > 1 / 8]), rep(1 / 8, 2), lwd = 2)

## Bayesian inference
# Exploring the beta density
library(manipulate)
pvals <- seq(0.01, 0.99, length = 1000)
manipulate(
    plot(pvals, dbeta(pvals, alpha, beta), type = "l", lwd = 3, frame = F),
    alpha = slider(0.01, 10, initial = 1, step = .5),
    beta = slider(0.01, 10, initial = 1, step = .5)
)

## 
pvals <- seq(0.01, 0.99, length = 1000)
x <- 13
n <- 20
myPlot <- function(alpha, beta) {
    plot(0 : 1, 0 : 1, type = "n", xlab = "p", ylab = "", frame = F)
    lines(pvals, dbeta(pvals, alpha, beta) / max(dbeta(pvals, alpha, beta)),
          lwd = 3, col = "darkred")
    lines(pvals, dbinom(x, n, pvals) / dbinom(x, n, x/n), 
          lwd = 3, col = "darkblue")
    lines(pvals, dbeta(pvals, alpha+x, beta+(n-x))/max(dbeta(pvals, alpha+x, beta+(n-x))), 
          lwd = 3, col = "darkgreen")
    title("red = prior, green = posterior, blue = likelihood")
}
manipulate(
    myPlot(alpha, beta),
    alpha = slider(0.01, 100, initial = 1, step = .5),
    beta = slider(0.01, 100, initial = 1, step = .5)
)

## HPD intervals
library(binom)
binom.bayes(13, 20, type = "highest")

pvals <- seq(0.01, 0.99, length = 1000)
x <- 13
n <- 20
myPlot2 <- function(alpha, beta, cl) {
    plot(pvals, dbeta(pvals, alpha+x, beta+(n-x)), type = "l", 
         lwd = 3, xlab = "p", ylab = "", frame = F)
    out <- binom.bayes(x, n, type = "highest",
                       prior.shape1 = alpha,
                       prior.shape2 = beta,
                       conf.level = cl)
    p1 <- out$lower
    p2 <- out$upper
    lines(c(p1, p1, p2, p2), c(0, dbeta(c(p1, p2), alpha+x, beta+(n-x)), 0),
          type = "l", lwd = 3, col = "darkred")
}
manipulate(
    myPlot2(alpha, beta, cl),
    alpha = slider(0.01, 10, initial = 1, step = .5),
    beta = slider(0.01, 10, initial = 1, step = .5),
    cl = slider(0.01, 0.99, initial = 0.95, step = .01)
)

## Tow group intervals
# x_oc = 132.86, s_oc = 15.34; x_c = 127.44, s_c = 18.23
sp <- sqrt((7*15.34^2 + 20*18.23^2) / (8 + 21 - 2))
132.86 - 127.44 + c(-1, 1)*qt(.975, 27) * sp * (1 / 8 + 1 / 21)^.5

data(sleep)
x1 <- sleep$extra[sleep$group == 1]
x2 <- sleep$extra[sleep$group == 2]
n1 <- length(x1)
n2 <- length(x2)
sp <- sqrt( ((n1 - 1) * sd(x1)^2 + (n2 - 1) * sd(x2)^2) / (n1 + n2 - 2))
md <- mean(x1) - mean(x2)
semd <- sp * sqrt(1 / n1 + 1 / n2)
md + c(-1, 1) * qt(.975, n1 + n2 - 2) * semd
t.test(x1, x2, paired = F, var.equal = T)$conf
# t.test(x1, x2, paired = F, var.equal = F)$conf


## Hypothesis testing
# Usage: Make decisions using data

# T test in R
library(UsingR)
data(father.son)
t.test(father.son$sheight - father.son$fheight)
