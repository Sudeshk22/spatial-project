spatial_data <- read.csv("spatial_data.csv")
log_likelihood <- function(params, i, c) {
  gamma <- params[1]
  delta <- params[2]
  
  log_lik <- sum(-gamma * c^delta + i * log(gamma * c^delta) - lfactorial(i))
  return(-log_lik)  # Return the negative log-likelihood for optimization
}
# Example data
#i <- c(i(s1), i(s2), i(s3), ..., i(sn_s))
#c <- c(c(s1), c(s2), c(s3), ..., c(sn_s))

#image estimate 
i <- spatial_data$Image.estimates

#exact weed count
c <-spatial_data$Exact.weed.counts 
# Initial parameter values
#initial_params <- c(initial_gamma_guess, initial_delta_guess)
initial_params <- c(3, 4)

# Run optimization
result <- optim(par = initial_params, fn = log_likelihood, i = i, c = c)

# Estimated parameters
gamma_hat <- result$par[1]
delta_hat <- result$par[2]
print(delta_hat)
print(gamma_hat)
