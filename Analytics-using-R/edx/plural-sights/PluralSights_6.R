getwd()

#ff: Memory-Efficient Storage of Large Data on Disk and Fast Access Functions
library(ff)

irisff <- read.table.ffdf(
              file="Iris.csv",
             FUN ="read.csv"
        )
class(irisff)
names(irisff)

#inspect
irisff[1:5,]

#linear regression model - bounded model
library(biglm)
model <- biglm(
  formula=Petal.Width ~ Petal.Length,
  data = irisff
)

summary(model)


# Create a scatterplot
plot(
  x = irisff$Petal.Length[], 
  y = irisff$Petal.Width[],
  main = "Iris Petal Length vs. Width",
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")

# Get y-intercept from model
b <- summary(model)$mat[1,1]

# Get slope from model
m <- summary(model)$mat[2,1]

# Draw a regression line on plot
lines(
  x = irisff$Petal.Length[],
  y = m * irisff$Petal.Length[] + b, 
  col = "red",
  lwd = 3)

# Predict new values with the model
predict(
  object = model,
  newdata = data.frame(
    Petal.Length = c(2, 5, 7),
    Petal.Width = c(0, 0, 0)))


