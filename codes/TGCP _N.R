# Load necessary libraries
library(MASS)  # For rlnorm
library(sp)     # For spatial data handling
spatial_data_CSV <- read.csv("spatial_data.csv")
# Function to simulate Poisson-distributed c(s) values based on intensity
simulate_counts <- function(intensity_values) {
  # Simulate Poisson-distributed c(s) values based on intensity
  simulated_counts <- rpois(length(intensity_values), lambda = intensity_values)
  return(simulated_counts)
}

# Function to generate intensity field (w(s)) based on predicted μ and σ^2
generate_intensity_field <- function(mu, sigma2, spatial_data) {
  # Simulate the intensity field (w(s)) based on the distribution defined by μ and σ^2
  # For example, assuming a log-normal distribution
  log_normal_samples <- rlnorm(n, meanlog = mu, sdlog = sqrt(sigma2))
  
  # Assign intensity values to spatial data
  spatial_data$intensity <- log_normal_samples
  
  return(spatial_data)
}

# Define the number of spatial locations (n)
n <- 100  # Replace with your desired value

# Assuming you have predicted μ and σ^2 values for the intensity field
# Replace with your actual predicted values
predicted_mu <- 5.022    # Predicted μ
predicted_sigma2 <- 5.25 # Predicted σ^2

# Generate spatial data (e.g., a grid of points or specific locations)
# Replace with your spatial data creation or loading
spatial_data <- data.frame(
  x = runif(n),
  y = runif(n)
)

# Generate the intensity field (w(s))
spatial_data <- generate_intensity_field(predicted_mu, predicted_sigma2, spatial_data)

# Simulate c(s) values based on the intensity field
spatial_data$c_values <- simulate_counts(spatial_data$intensity)

# Print or use the simulated c(s) values
print(spatial_data)


## plotting the predicted values of the weed count against the actual weed count values 
# Assuming you have predicted_values and actual_values vectors
# Replace these with your actual data
predicted_values <- spatial_data$c_values  # Simulated counts
actual_values <- spatial_data_CSV$Exact.weed.counts  # Actual weed counts

# Create a scatter plot
plot(actual_values, predicted_values, 
     ylab = "Predicted Counts (TGCP)",
     xlab = "Actual Counts",
     main = "Scatter Plot of Predicted vs. Actual Weed Counts")

# Add a 45-degree reference line for comparison
abline(0,1, col = "red")


### Drawing the QQ plot 
# Assuming you have predicted_values and actual_values vectors
predicted_values <- spatial_data$c_values  # Simulated counts
actual_values <- spatial_data_CSV$Exact.weed.counts  # Actual weed counts

# Create a Quantile-Quantile (Q-Q) plot
qqplot(actual_values, predicted_values, 
       ylab = "Predicted Counts (TGCP)",
       xlab = "Actual Counts",
       main = "Quantile-Quantile (Q-Q) Plot")

# Add a reference line for comparison
abline(0, 1, col = "red")

### mean Square error of TGCP model
## calculating the mean square error by the prediction using the TGCP model 
# Calculate residuals
residuals <-actual_values - predicted_values

# Square residuals
squared_residuals <- residuals^2

# Calculate MSE
mse <- mean(squared_residuals)

# Print the MSE
cat("Mean Squared Error (MSE):", mse, "\n")

# MSE of the TGCP model is 20724.17

