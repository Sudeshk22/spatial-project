mydir <- "C:/Users/sudesh yadav/OneDrive - IIT Kanpur/Desktop/spatial project"
setwd(mydir)

# Load the CSV data into a data frame
data <- read.csv("spatial_data.csv")


# Create a histogram
hist(data$Exact.weed.counts, breaks = 20, col = "skyblue", border = "black", 
     xlab = "Exact Weed Count", ylab = "Frequency", 
     main = "Histogram of Exact Weed Count")
