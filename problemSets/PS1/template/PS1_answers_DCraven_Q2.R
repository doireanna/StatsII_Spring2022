library(tidyverse)
set.seed(123)

data <- data.frame(x = runif(200, 1, 10))     # Random x's
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)# y = f(x), beta0=0, beta1=2.75
                                              # normally distributed errors

plot(data$x, data$y,  ylab = "Y", xlab = "X")

like_fn <- function(theta, y, X){     # Arguments are:
                                     # 1. theta (vector of parameters)
                                     # 2. y = f(x)
                                     # 3. X = cbind(1, data$x)
  
  n <- nrow(X)                       # Number of x values
  k <- ncol(X)                       # Number of betas
  beta <- theta[1:k]                 # Dataframe of the betas in theta
  sigma_sq <- theta[k+1]^2           # Last element in theta, squared
  m <- y - X%*%beta                  # %*% multiplies 2 matrices 
  
                                     # LLF for normal distribution:
  logl <- -0.5*n*log(2*pi) - 0.5*n*log(sigma_sq) - ((t(m)%*%m)/(2*sigma_sq))
                                     # t(m) transposes m to then square
  return(-logl)
}

OLS <- optim(fn = like_fn, par = c(1, 1, 1) , hessian = TRUE,
             y = data$y , X = cbind(1, data$x), method = "L-BFGS-B",
             lower = 0)
OLS

model_summary <- summary(lm (y ~ x, data))
model_summary$sigma                  # Get sigma
(model_summary)

# View the wireframe:

surface <- list()     
e <- 0
for (beta in seq(0, 5, 0.1)){
  for (sigma in seq(0.1, 5, 0.1)){
    e <- e+1
    logL <- like_fn(theta = c(0, beta, sigma), y = data$y,
                   X = cbind(1, data$X))
    surface[[e]] <- data.frame(beta = beta, sigma = sigma, logL = -logL)
  }
}
surface <- do.call(rbind, surface)
library(lattice)
wireframe (logL ~ beta*sigma, surface, shade = TRUE)






