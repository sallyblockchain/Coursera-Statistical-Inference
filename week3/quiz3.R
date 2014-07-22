## Quiz 3.
# Problem 1.
x_bar <- 1100
s <- 30
n <- 9
alpha <- 0.05
ts <- qt(1 - alpha / 2, n - 1) # 2.306004
round(x_bar + c(-1, 1) * ts * s / sqrt(n)) # 1077 1123

# Problem 2.
x_bar <- -2
n <- 9
alpha <- 0.05
ts <- qt(1 - alpha / 2, n - 1) # 2.306004
s <- -x_bar*sqrt(n) / ts
s # 2.601903

# Problem 3.
# Independent tests: not related participants
# Paired tests: related participants, same group uses 2 different tests
# A paired interval.

# Problem 4.
n_x <- 10 
n_y <- 10
x_bar <- 5 # old_system
y_bar <- 3 # new_system
var_x <- 0.6
var_y <- 0.68
alpha <- 0.05
sp_2 <- ((n_x - 1)*var_x + (n_y - 1)*var_y) / (n_x + n_y - 2)
sp <- sqrt(sp_2)
ts <- qt(1 - (alpha/2), n_x + n_y - 2)
round((y_bar - x_bar) + c(-1, 1) * ts * sp * (sqrt(1/n_x + 1/n_y)), 2) 
# -2.75 -1.25

# Problem 5.
# 90% confidence interval gives a lower t-value then 95% confidence interval. 
# => The interval will be narrower.

# Problem 6.
n_x <- 100
n_y <- 100
x_bar <- 6
y_bar <- 4
s_x <- 2
s_y <- 0.5
alpha <- 0.05
sp_2 <- ((n_x - 1)*s_x^2 + (n_y - 1)*s_y^2) / (n_x + n_y - 2)
sp <- sqrt(sp_2)
ts <- qt(1 - (alpha/2), n_x + n_y - 2)
round((x_bar - y_bar) + c(-1, 1) * ts * sp * (sqrt(1/n_x + 1/n_y)), 2) 
# 1.59 2.41 => The new system appears to be effective. 

# Problem 7.
n_x <- 9
n_y <- 9
x_bar <- -3
y_bar <- 1
s_x <- 1.5
s_y <- 1.8
alpha <- 0.1
sp_2 <- ((n_x - 1)*s_x^2 + (n_y - 1)*s_y^2) / (n_x + n_y - 2)
sp <- sqrt(sp_2)
ts <- qt(1 - (alpha/2), n_x + n_y - 2)
round((x_bar - y_bar) + c(-1, 1) * ts * sp * (sqrt(1/n_x + 1/n_y)), 3) 
# -5.364 -2.636

# Problem 8.
# Discrete distribution with an unknown parameter: MLE gives 
# the value of the parameter that makes the observed data most probable given
# the model.

# Problem 9.
# Bayesian inference: posterior is direct propotion to likelihood*prior
