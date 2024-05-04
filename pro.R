library(quadprog)
#Z_1(t)   Z_2(t)
set.seed(100) 
T <- 1         
alpha <- 1.8   
dt <- 0.0002   # step
n <- T / dt    
t_seq <- seq(0, T, by=dt) 
v(t) = t^alpha
var_function <- function(t, alpha) {
  return(t^alpha)
}
create_gaussian_process <- function(t_seq, var_function, alpha) {
  n <- length(t_seq)
  increments <- rnorm(n - 1, mean = 0, sd = sqrt(diff(var_function(t_seq, alpha))))
  process <- c(0, cumsum(increments))
  return(process)
}
# Z_1(t)   Z_2(t)
Z_1 <- create_gaussian_process(t_seq, var_function, alpha)
Z_2 <- create_gaussian_process(t_seq, var_function, alpha)
Z=c(Z_1,Z_2)
#c
c1 <- runif(1, min=0, max=0.01)
c2 <- runif(1, min=0, max=0.01)c <- c(c1, c2)
#rho A
rho <- runif(1, min=0, max=1)
A <- matrix(c(1, rho, 0, sqrt(1 - rho^2)), nrow=2)
Z <- cbind(Z_1, Z_2)
X <- Z %*% A 
set.seed(100)
u_values <- seq(0,0.3,by=0.00001)
joint_exceeds_prob <- numeric(length(u_values))
first_company_exceeds_prob <- numeric(length(u_values))
p3_prob <- numeric(length(u_values))
P4<- numeric(length(u_values))
for (i in seq_along(u_values)) {
  u <- u_values[i]
  a <- runif(1, min=-1, max=rho)  
  ua <- u * a
  joint_exceeds <- (X[,1] - c1 * T > u) & (X[,2] - c2 * T > u)
  first_company_exceeds <- (X[,1] - c1 * T > u)
  joint_exceeds_prob[i] <- mean(joint_exceeds)
  first_company_exceeds_prob[i] <- mean(first_company_exceeds)}
delta <- 0.0001
selected <- (X[,1] >= c1*T - delta) & (X[,1] <= c1*T + delta)
prob <- sum(X[,2][selected] > c2*T) / sum(selected)
P4<- prob*first_company_exceeds_prob
# no 0
ratios <- ifelse(P4 > 0,joint_exceeds_prob/P4, 0)
# pic2
plot(u_values, ratios, type = 'l', col = 'black', xlab = 'u', ylab = '概率比值')
# pic1
plot(u_values, joint_exceeds_prob, type = 'l', col = 'black', lwd = 1,
     xlab = 'u', ylab = '破产概率',
     ylim = range(c(joint_exceeds_prob, first_company_exceeds_prob))) 
lines(u_values, first_company_exceeds_prob, col = 'black', lty = 2, lwd = 1)
legend("topright", legend = c("同时超过阈值的概率", "第一家公司超过阈值的概率"), 
       col = c("black", "black"), lty = c(1, 2), lwd = 1,cex = 0.75)