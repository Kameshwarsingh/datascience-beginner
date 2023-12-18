#https://github.com/bnwicks/Machine-Learning/blob/master/Distance%2C%20Knn%2C%20Cross-validation%2C%20and%20Generative%20Models.R

library(dslabs)
library(tidyverse)
library(caret)

data("heights")
glimpse(heights)



set.seed(1)
y <- heights$sex
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights %>% slice(test_index)
train_set <- heights %>% slice(-test_index)


glimpse(train_set)
glimpse(test_set)
glimpse(y)
#k_range=seq(1, 101, 3)
#k_range


knn_fit <- knn3(sex ~ height, data = train_set, k=5)
y_hat_knn <- predict(knn_fit,test_set, type = "class")
confusionMatrix(data=y_hat_knn,reference=test_set$sex)$overall["Accuracy"]

ks <- seq(1, 101, 3)
ks
F1_score <- sapply(ks, function(k){
  set.seed(1)
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  test_set <- heights %>% slice(test_index)
  train_set <- heights %>% slice(-test_index)
  
  knn_fit <- knn3(sex ~ height, data = train_set, k=k)
  
  y_hat_knn <- predict(knn_fit,test_set, type = "class") %>% 
    factor(levels = levels(test_set$sex))
  
  #F1_score
  F_meas(data = y_hat_knn, reference = test_set$sex)
  })

F1_score
plot(ks, F1_score)
max(F1_score)

print(1 + (which(F1_score==max(F1_score))[[1]] -1 ) *3 )





library(dslabs)
data("tissue_gene_expression")
ks <- seq(1, 11, 2)

set.seed(1)
F_1 <- sapply(ks, function(k){
 
  #create training index (rather than test index) as suggested by comments
  train_index <- createDataPartition(tissue_gene_expression$y, p = 0.5, list = FALSE)
  # split original data set into x and y
  x <- tissue_gene_expression$x
  y <- tissue_gene_expression$y
  # split x into train and test sets
  train_set_x = x[train_index,]
  test_set_x = x[-train_index,]
  # split y into train and test sets
  train_set_y = y[train_index]
  test_set_y = y[-train_index]
  # merge x and y train sets as a list (as per original data set)
  train_set = list('x' = train_set_x, 'y' = train_set_y)
  # merge x and y test sets
  test_set = list('x' = test_set_x, 'y' = test_set_y)
  
  fit <- knn3(y ~ ., data = (as.data.frame(train_set)), k = k)
  y_hat <- predict(fit, (as.data.frame(test_set)), type = "class") %>% factor(levels = levels(y))
  confusionMatrix(data=y_hat, reference=test_set_y)$overall["Accuracy"]
}) 

#plot(ks, F_1) 
#max(F_1)
F_1
ks


