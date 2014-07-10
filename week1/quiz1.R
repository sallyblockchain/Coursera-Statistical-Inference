## Quiz 1
# Problem 1. 
paub <- 0.17
pa <- 0.12
panb <- 0.06
pb <- paub + panb - pa
pb # 0.11
# Problem 2.
pbeta(0.75, 1, 1) # 0.75
# Problem 3. 
# p*(-X) + (1-p)*Y=0
# p*X = (1-p)*Y
# p/(1-p) = Y/X
# Problem 4.
ex <- 1/2*(1-1)
var <- (1^2+1^2)/2
var # 1
# Problem 5.
# sigma_1^2/n_1 + sigma_2^2/n_2
# Problem 6.
# X ~ (mu, sigma^2)
# Z ~ (0, 1)
# Problem 7.
# Yes, it is 0 due to symmetry.
# Problem 8.
x <- 1:4
p <- x/sum(x)
temp <- rbind(x, p)
rownames(temp) <- c("X", "Prob")
temp
mean <- sum(temp[1, ]*temp[2, ])
mean # 3
