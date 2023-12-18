library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

glimpse(train)

#Sepal.Length
cutoff <- seq(min(train$Sepal.Length), max(train$Sepal.Length), by=0.1)
cutoff
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})
glimpse(accuracy)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#Sepal.width
cutoff <- seq(min(train$Sepal.Width), max(train$Sepal.Width), by=0.1)
cutoff
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})
glimpse(accuracy)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


#Petal.Length
cutoff <- seq(min(train$Petal.Length), max(train$Petal.Length), by=0.1)
cutoff
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})
glimpse(accuracy)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#Petal.width
cutoff <- seq(min(train$Petal.Width), max(train$Petal.Width), by=0.1)
cutoff
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor")
  mean(y_hat == train$Species)
})
glimpse(accuracy)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#########################################################################################################33
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	


#test
cutoff <- c(1.7)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor")
  mean(y_hat == test$Species)
})
glimpse(accuracy)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


plot(iris,pch=21,bg=iris$Species)


