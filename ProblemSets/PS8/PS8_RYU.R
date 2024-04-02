install.packages("nloptr")
library(nloptr)


######### Q4.
# Set the seed of the randomnumber generator by issuing the (R) command set.seed(100)
set.seed(100)

# X is a matrix of dimension N = 100, 000 by K = 10 containing normally distributed 
# random numbers, except the first column which should be a column of 1’s
N <- 100000
K <- 10
X <- matrix(rnorm(N * (K - 1)), nrow = N, ncol = K - 1)
X <- cbind(rep(1, N), X)
dim(X)

# Set epsilon
sigma <- 0.5
sigma_sq <- sigma^2
eps <- rnorm(N, mean = 0, sd = sqrt(sigma_sq))
length(eps)

# Set beta
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

length(beta)
beta
dim(beta)

# Generate Y
Y <- X %*% beta + eps
head(Y)


######### Q5.
# Compute beta_hat_OLS
beta_hat_OLS <- solve(t(X) %*% X) %*% t(X) %*% Y

true_beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
beta_comparison <- data.frame(True_Beta = true_beta, Estimated_Beta = beta_hat_OLS)
beta_comparison

# Very little difference. Rounding up to the decimal point yields a nearly identical value.









######### Q6.
# Compute beta_hat_OLS using gradient descent

library(nloptr)
alpha <- 0.0000003
maxiter <- 1000000

objfun <- function(beta2,Y,X) {
  return ( sum((Y-X%*%beta2)^2) )
}

gradient <- function(beta2,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta2)) )
}


dim(X)
beta2 <- runif(dim(X)[2])
beta2

set.seed(100)

beta2.All <- matrix("numeric",length(beta2),maxiter)

iter  <- 1
beta0 <- 0*beta2
while (norm(as.matrix(beta0)-as.matrix(beta2))>1e-8) {
  beta0 <- beta2
  beta2 <- beta0 - alpha*gradient(beta0,Y,X)
  beta2.All[,iter] <- beta2
  if (iter%%10000==0) {
    print(beta2)
  }
  iter <- iter+1
}

print(iter)
print(paste("The minimum of f(beta2,Y,X) is ", beta2, sep = ""))














######### Q7-1.
# Compute beta_hat_OLS using nloptr's L-BFGS algorithm

library(nloptr)
objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
}

gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}


beta0 <- runif(dim(X)[2]) 
beta0



options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8,"maxeval"=1e3)
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)




######### Q7-2.
# Compute beta_hat_OLS using Nelder-Mead algorithm



library(nloptr)
objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
}


beta0 <- runif(dim(X)[2]) 



options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8,"maxeval"=1e3)
res <- nloptr( x0=beta0,eval_f=objfun,opts=options,Y=Y,X=X)
print(res)


# Very similar, but not completely identical, values were derived.

# Result of Computing beta_hat_OLS using nloptr's L-BFGS algorithm
# Optimal value of objective function:  24990.8396899258 
# Optimal value of controls: 1.501052 -1.00083 -0.251648 0.7490406 3.500553 -2.000819 0.4987148 1.002827 1.24651 2.001001

# Result of Computing beta_hat_OLS using Nelder-Mead algorithm
# Current value of objective function:  24992.0556652445 
# Current value of controls: 1.500973 -0.9984383 -0.2511872 0.7477687 3.499967 -2.001487 0.4983476 1.001078 1.246084 2.00035






######### Q8.
# Compute beta_hat_MLE using nloptr's L-BFGS algorithm

library(nloptr)
objfun  <- function(theta,Y,X) {
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

gradient <- function (theta,Y,X) {
  grad     <- as.vector(rep(0,length(theta)))
  beta     <- theta [1:(length(theta)-1)]
  sig      <- theta [length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%beta)/(sig^2)
  grad[length(theta)]       <- dim(X)[1]/sig-crossprod (Y-X%*%beta)/(sig^3)
  return ( grad )
}



theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
theta0 <- append(as.vector(summary(lm(Y~X-1))$coefficients[,1]),runif(1))


options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)
result <- nloptr( x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)

betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]
betahat
# 1.5010518 -1.0008296 -0.2516480  0.7490406  3.5005531 -2.0008185  0.4987148  1.0028269  1.2465102 2.0010012

sigmahat
# 0.3277259






# Q9.
# Compute beta_hat_OLS using lm()
print(summary(lm(Y~X-1)))




install.packages("modelsummary")
library(modelsummary)
install.packages("broom")
library(broom)

model <- lm(Y~X-1)
modelsummary(list(model), output = "regression.tex")
















