mydir <- "C:/Users/sudesh yadav/OneDrive - IIT Kanpur/Desktop/spatial project"
setwd(mydir)

# Load the CSV data into a data frame
data <- read.csv("spatial_data.csv")


# Create a histogram
hist(data$Image.estimates, breaks = 20, col = "skyblue", border = "black", 
     xlab = "Image estimates", ylab = "Frequency", 
     main = "Histogram of Image estimate")
