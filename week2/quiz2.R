## Quiz 2
# Problem 1.
# P(G2 | G1^c) = P(G2 and G1^c)/P(G1^c) = (0.8-0.7)/(1-0.8)=0.5
# Problem 2.
# P(+|D) = 0.75, P(-|D^c) = 0.52 => P(+|D^c) = 1 - 0.52 = 0.48
# P(D) = 0.3 => P(D^c) = 0.7
# P(D|+) = P(+|D)P(D) / ( P(+|D)P(D) + P(+|D^c)P(D^c) )
0.75*0.3 / (0.75*0.3 + 0.48*0.7) # = 0.4010695
# Problem 3.
z <- (70 - 80)/ 10 
pnorm(-abs(z)) # 0.1586553
# Problem 4.
mu <- 1100
s <- 75
mu + qnorm(0.95)*s # 1223.364
qnorm(0.95, mean = 1100, sd = 75) # 1223.364
# Problem 5.
mu <- 1100
s <- 75
n <- 100
mu + qnorm(0.95)*s/sqrt(n) # 1112.336
# Problem 6.
choose(5, 4)*0.5^5 + choose(5, 5)*0.5^5 # 0.1875
# Problem 7.
# mu=15, s=sigma/sqrt(100)=10/10=1, 
# P(-1 <= z <= 1)
z <- 1
1 - pnorm(-abs(z))*2 # 0.6826895
# Problem 8.
# The law of large numbers
# sample mean(approx.)=mu=0.5
mean(rnorm(1e+07, mean = 0.5, sd = sqrt(1/12))) # 0.4999649
# Problem 9.
sqrt(1/12)/sqrt(100) # 0.02886751
# Problem 10.
ppois(10, lambda = 5 * 3) # 0.1184644
# Problem 11
choose(9, 3) # 84
