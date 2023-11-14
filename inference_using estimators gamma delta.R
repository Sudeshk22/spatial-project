# Load the required library
library(ggplot2)

# Load your spatial data CSV file
data <- read.csv("spatial_data.csv")
data$Exact.weed.counts
data$Image.estimates
# Use Parameters as estimated  previously 
gamma <- 3.90625

delta <- 0.725

# Calculate lambdas
lambda_values <- gamma * ((data$Exact.weed.counts )^delta)

#checking anout the classes of datset
class((data$Exact.weed.counts ))
class(delta)


# Create a data frame with lambda and image_estimates
lambda_data <- data.frame(lambda = lambda_values, image_estimates = data$Image.estimates)

# Create a pair plot
plot(lambda_data, main = "Pair Plot of Lambdas and Image Estimates")
abline(0, 1, col = "red")

# Calculate lambdas
lambda_values <- gamma * ((data$Exact.weed.counts)^delta)

# Create a Q-Q plot
qqplot(lambda_values, data$Image.estimates, main = "Q-Q Plot of Lambdas vs. Image Estimates")
abline(0, 1, col = "red")  # Add a reference line
