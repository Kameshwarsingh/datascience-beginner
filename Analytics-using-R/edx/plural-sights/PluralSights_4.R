# Creating Data Visualization

# Set the working directory
setwd("C:/Pluralsight")

# Read the CSV file
cars <- read.csv("Cars.csv")
nrow(cars)
# Load the ggplot2 library
library(ggplot2)

# Create a frequency bar chart - number of cars for given transmission type
ggplot(
  data = cars, 
  aes(x = Transmission)) + 
  geom_bar() +
  ggtitle("Count of Cars by Transmission Type") +
  xlab("Transmission Type") +
  ylab("Count of Cars")

# Create a histogram - count of cars in given bin
ggplot(
  data = cars, 
  aes(x = Fuel.Economy)) +
  geom_histogram(
    bins = 20) +
  ggtitle("Distribution of Fuel Economy") +
  xlab("Fuel Economy (mpg)") +
  ylab("Count of Cars")

# Create a density plot
ggplot(
  data = cars, 
  aes(x = Fuel.Economy)) +
  geom_density() +
  ggtitle("Distribution of Fuel Economy") +
  xlab("Fuel Economy (mpg)") +
  ylab("Density")

# Create a scatterplot
ggplot(
  data = cars, 
  aes(
    x = Cylinders,
    y = Fuel.Economy)) +
  geom_point() +
  ggtitle("Fuel Economy by Cylinders") +
  xlab("Number of Cylinders") +
  ylab("Fuel Economy (mpg)")

