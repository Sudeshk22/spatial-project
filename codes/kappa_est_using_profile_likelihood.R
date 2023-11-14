# Step 1: Generate data
set.seed(123)  # Set a seed for reproducibility
n <- 100  # Number of data points
s <- rnorm(n)  # Simulate y(s) from a standard normal distribution
h <- rnorm(n)  # Simulate h

# Step 2: Define the log-likelihood function
log_likelihood <- function(kappa, data_s, data_h) {
  # Calculate the correlation based on the provided formula
  corr <- exp(-sqrt(data_h^2) / kappa)
  # Calculate the log-likelihood based on the correlation
  log_lik <- sum(log(dnorm(data_s, mean = 0, sd = 1) * dnorm(data_s + sqrt(kappa) * data_h, mean = 0, sd = 1) * corr))
  return(-log_lik)  # Return the negative log-likelihood for maximization
}

# Step 3: Profile likelihood estimation of kappa
kappa_values <- seq(0.01, 2, by = 0.01)  # Values of kappa to profile
profile_likelihood <- numeric(length(kappa_values))

for (i in 1:length(kappa_values)) {
  profile_likelihood[i] <- log_likelihood(kappa_values[i], s, h)
}

# Find the estimated kappa that maximizes the profile likelihood
estimated_kappa <- kappa_values[which.min(profile_likelihood)]

# Print the estimated kappa
cat("Estimated kappa:", estimated_kappa, "\n")
