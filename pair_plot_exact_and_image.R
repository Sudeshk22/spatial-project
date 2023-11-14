# Load the CSV data into a data frame
data <- read.csv("spatial_data.csv")
# Create a scatterplot between "Image Estimate" and "Exact Weed Count"
#png("pair_plot.png", width = 800, height = 600)
plot(data$Exact.weed.counts, data$Image.estimates, 
     col = "black",
     pch = 19, # Point type for the scatterplot
     xlab = "Exact Weed Count ", ylab = "Image Estimate",
     main = "Scatterplot of Image Estimate vs. Exact Weed Count")

# Perform linear regression to get the slope and   intercept
lm_model <- lm(Exact.weed.counts ~ data$Image.estimates, data = data)
slope <- coef(lm_model)[2]
intercept <- coef(lm_model)[1]

 abline(a = 0, b = slope, col = "red", lwd = 2) # Add line of best fit

# Display the plot
#dev.off()

