# Load required libraries
library(gstat)
library(ggplot2)
library(sp)
mydir <- "C:/Users/sudesh yadav/OneDrive - IIT Kanpur/Desktop/spatial project"

setwd(mydir)



# Read the CSV file
spatial_data <- read.csv("spatial_data.csv")

spatial_data_sp <- SpatialPointsDataFrame(
  coords = spatial_data[, c("Eastings..meter.", "Northing.meter.")],  # Specify X and Y columns
  data = spatial_data  # Include all other data columns
)


# Create a variogram model
variogram_model <- variogram(Exact.weed.counts ~ 1, locations = spatial_data_sp, width = 50)

# Plot the empirical variogram
plot(variogram_model, pch = 19, col = "blue", main = "Empirical Variogram with Exponential Model")
#abline(h = 0, col = "red", lty = 2)  # Add a reference line at y = 0

# Fit an exponential model to the variogram
variogram_fit <- fit.variogram(variogram_model, model = vgm(psill = 4500, nugget = 0, model = "Exp", range = 50))

# Add the fitted model to the plot
plot(variogram_model, variogram_fit, col = "black", main = "Empirical Variogram and Exponential Model")

# Display the plot
# Estimate the mean from the variogram model
estimated_mean <- mean(variogram_fit$np)

# Print the estimated mean
print(estimated_mean)

#calculated mean shold be 80m



