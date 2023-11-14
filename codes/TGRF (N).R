# Simulated data for demonstration (replace with your actual data)
set.seed(123)
n <- 100  # Number of data points
data <- read.csv("spatial_data.csv")
S <- data.frame(x = data$Eastings..meter., y = data$Northing.meter., c = data$Exact.weed.counts)
T <- S  # For demonstration purposes, we use S as both observed and new locations

# Define the correlation function based on the TGRF model
correlation_function <- function(h, kappa) {
  return(exp(-sqrt(h[1]^2 + h[2]^2) / kappa))
}

# Function for parameter estimation (e.g., MLE for kappa)
estimate_kappa <- function(data) {
  # Your parameter estimation code here
  kappa <- 1.13  # Replace with your actual estimation result
  return(kappa)
}

# Function to predict image estimates
predict_image <- function(data, kappa) {
  n <- nrow(data)
  predicted_values <- numeric(n)
  
  for (i in 1:n) {
    y_i <- rnorm(1)  # Simulate a standard normal random variable
    h <- as.matrix(data[i, c("x", "y")]) - data[, c("x", "y")]
    corr_values <- correlation_function(h, kappa)
    weighted_values <- data$c * corr_values
    predicted_values[i] <- sum(weighted_values) / sum(corr_values)
  }
  
  return(predicted_values)
}

# Set up k-fold cross-validation
k <- 5  # Number of folds
folds <- split(1:nrow(S), sample(1:k, nrow(S), replace = TRUE))

# Initialize a vector to store predictions and actual values
all_predictions <- c()
all_actual <- c()

# Perform cross-validation
for (fold in 1:k) {
  train_indices <- unlist(folds[-fold])
  test_indices <- unlist(folds[fold])
  
  train_data <- S[train_indices, ]
  test_data <- S[test_indices, ]
  
  # Estimate kappa from the training data
  kappa <- estimate_kappa(train_data)
  
  # Predict image estimates for the test data
  predictions <- predict_image(test_data, kappa)
  
  # Store the predictions and actual values
  all_predictions <- c(all_predictions, predictions)
  all_actual <- c(all_actual, test_data$c)
}

# Create a pair plot between predicted and actual values
pair_plot_data <- data.frame(Actual = all_actual, Predicted = all_predictions)
plot(pair_plot_data$Actual, pair_plot_data$Predicted, xlab = "Actual Values", ylab = "Predicted Values")
abline(a = 0, b = 1, col = "red")

## creating the Q- Q plot between predictive and actual weed count 
# Assuming you have the predicted values and the actual "exact weed counts" in vectors
predicted_values <- all_predictions
exact_counts <- all_actual     

# Create a Q-Q plot
qqplot(exact_counts, predicted_values,
       xlab = "Quantiles of Actual Weed Counts",
       ylab = "Quantiles of Predicted Values",
       main = "Quantile-Quantile (Q-Q) Plot")

# Add a 45-degree reference line
abline(0, 1, col = "red")

## calculating the mean ssquare errror by the prediction using the TGCP model 
# Calculate residuals
residuals <- all_actual - all_predictions

# Square residuals
squared_residuals <- residuals^2

# Calculate MSE
mse <- mean(squared_residuals)

# Print the MSE
cat("Mean Squared Error (MSE):", mse, "\n")

# MSE of the TGCP model is 2894.996