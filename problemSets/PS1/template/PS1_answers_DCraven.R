library(tidyverse)
set.seed(123)

# Generate 1000 random variable, Cauchy distribution 
rand_cau <- rcauchy(1000, location = 0, scale = 1)

my_ks_test <- function(data, alpha){  # Argument = data and significance level
  
  ECDF <- ecdf(data)                  # Use r's ecdf function to calculate 
                                      # a CD function for the data
  ECD_val <- ECDF(data)               # Apply the CD function to the data
  D <- max(abs(ECD_val - pnorm(data)))# D is the difference between the 
                                      # calculated ECDF and normal CDF
  
  k = 1000000                         # Summation will run from 1 to 1 million
  sigma = 0                           # Set initial value of summation
  for(i in 1:k){                      # Iterate from 1 to 100
                                      # Sum terms:
    sigma = sigma + exp((-(2 * i - 1)^2 * pi^2) / ((8 * D)^2))
  }                                   
  p_value <- sqrt(2*pi)/D * sigma     # Final calculation for p-value
  
  if(p_value < alpha){                # Include a conclusion
    conc <- sprintf("Reject H0 at the %f significance level", alpha)
  }                                   # sprintf combines a string with a float
  else{
    conc <- sprintf("Cannot reject H0 since p-value is > %f", alpha)
  }
  output <- c("H0 is that the data are normally distributed.",
              sprintf("D = %f", D), sprintf("p-value = %f", p_value), conc)
  return(output)
}

my_ks_test(rand_cau, 0.05)

answer <- as.data.frame(my_ks_test(rand_cau, 0.05))
write.csv(answer, file = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS1/Answer.r", row.names = FALSE)



# Compare to r's built-in function
ks.test(rand_cau, "pnorm")
