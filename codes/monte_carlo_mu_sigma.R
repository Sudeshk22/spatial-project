# Number of samples you want to generate
n_samples <- 100

# Generate random samples from a standard normal distribution
standard_normal_samples <- rnorm(n_samples)
# Define your desired mean and variance
mu <- 5.0
sigma_squared <- 4.0

# Calculate the parameters of the gamma distribution
k <- (mu^2) / sigma_squared
theta <- sigma_squared / mu

# Transform the standard normal samples to gamma samples
gamma_samples <- rgamma(n_samples, shape = k, rate = theta)
# Load the 'stats' package for the gamma distribution PDF
library(stats)

likelihood <- function(parameters, data) {
  k <- parameters[1]
  theta <- parameters[2]
  prod(dgamma(data, shape = k, scale = theta))
}
# Number of Monte Carlo samples
n_monte_carlo_samples <- 10000

# Initialize arrays to store parameter samples
parameter_samples <- matrix(0, nrow = n_monte_carlo_samples, ncol = 2)

# Perform Monte Carlo sampling
for (i in 1:n_monte_carlo_samples) {
  # Sample parameters from some prior distribution (if applicable)
  # For simplicity, you can use uniform priors in this example
  k_sample <- runif(1, 0.1, 10.0)
  theta_sample <- runif(1, 0.1, 10.0)
  
  # Calculate likelihood for the sampled parameters
  likelihood_sample <- likelihood(c(k_sample, theta_sample), gamma_samples)
  
  # Store the parameter samples
  parameter_samples[i, 1] <- k_sample
  parameter_samples[i, 2] <- theta_sample
}

# Calculate the estimated mean and variance
estimated_mu <- mean(parameter_samples[, 2] / parameter_samples[, 1])
estimated_sigma_squared <- var(parameter_samples[, 2] / parameter_samples[, 1])

print(estimated_mu)
print(estimated_sigma_squared)
