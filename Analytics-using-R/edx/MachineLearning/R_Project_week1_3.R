library(caret)


#load data
data(iris)


#exclude "setosa"
iris <- iris[-which(iris$Species=='setosa'),]
summary(iris)

y <- iris$Species
summary(y)

set.seed(2, sample.kind="Rounding")

#partition
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)

test <- iris[test_index,]
train <- iris[-test_index,]

summary(test)
summary(train)



#load data
data(iris)
#you take the Iris data, then you subset the data and then you aggregate the data"
iris %>%
  subset(Sepal.Length > 5) %>%
    aggregate(. ~ Species, ., mean)


#########################################

# Initialize `x`
x <- c(0.109, 0.359, 0.63, 0.996, 0.515, 0.142, 0.017, 0.829, 0.907)

# Compute the logarithm of `x`, return suitably lagged and iterated differences, 
# compute the exponential function and round the result
round(exp(diff(log(x))), 1)

# Import `magrittr`
library(magrittr)

# Perform the same computations on `x` as above
x %>% log() %>%
  diff() %>%
  exp() %>%
  round(1)


