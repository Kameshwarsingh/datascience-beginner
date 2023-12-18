library("ggpubr")

set.seed(1)

n <- 100

##create 2*2 matrix 
?matrix
#[matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)]
Sigma <- 9*matrix( c(1.0, 0.5, 0.5, 1.0), 2, 2)
print(Sigma)

#Multivariate Normal Distribution
?MASS::mvrnorm
# mvrnorm(n = 1, mu, Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
# n	: the number of samples required.
# mu	: a vector giving the means of the variables.
# Sigma	: a positive-definite symmetric matrix specifying the covariance matrix of the variables.


dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#check mean of data-frame, must be close to 69
summary(dat)

#check correlation between x and y, must be close to 0.5
#?cor
cor(dat$x,dat$y)



##partition data set
set.seed(1)


iteration=100
rmse_array <- numeric(iteration)
print(rmse_array)

for (count in 1:iteration){
                  #use dat$y - to generate test/train data indeces
                  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
                  train_set <- dat %>% slice(-test_index)
                  test_set <- dat %>% slice(test_index)
                  
                  #glimpse(train_set)
                  #glimpse(test_set)
                  
                  
                  ## train linear model  - predict "y", "x" is independent/feature/variate
                  fit <- lm(y ~ x, data = train_set)
                  #fit
                  
                  #predict
                  y_hat <- predict(fit,test_set)
                  
                  #RMSE - loss function
                  rmse <- sqrt(mean((y_hat - test_set$y)^2))
                  rmse_array[count] <- rmse
                 # print(rmse)
                }

          
print(rmse_array)
mean(rmse_array)
sd(rmse_array)



### Comprehension Check: Linear Regression
library(caret)
library(tidyverse)

# Question 1

y_rmse <- c(1:100)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
dat

set.seed(1)
for(i in 1:100) # For Loop (Should Use Replicate)
{    
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  # Train linear model
  fit <- lm(y ~ x, data = train_set)
  fit
  
  # Loss Function
  y_hat <- predict(fit, test_set)
  y_rmse[i] <- sqrt(mean((y_hat - test_set$y)^2))
  print(i)
}

y_rmse
mean(y_rmse)
sd(y_rmse)

#https://raw.githubusercontent.com/bnwicks/Machine-Learning/master/Comprehension%20Check%20Linear%20Regression.R



# Question 2 
set.seed(1)
myRMSE <- function(size)
{
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))

  RMSE <- replicate(n = size, {
    
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    
    # Train linear model
    fit <- lm(y ~ x, data = train_set)
    
    # Loss Function
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  list(mean(RMSE),sd(RMSE))
}

n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
f<-sapply(n, myRMSE)
f






set.seed(1)    # if R 3.6 or later, set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)

res <- sapply(n, function(n){
  
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  
  c(avg = mean(rmse), sd = sd(rmse))
})
res


















# Question 4
set.seed(1)
myRMSE <- function(size)
{
  #set.seed(1)
  Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  set.seed(1)
  RMSE <- replicate(n = size, {
    
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    
    # Train linear model
    fit <- lm(y ~ x, data = train_set)
    
    # Loss Function
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  print(RMSE)
  list(mean(RMSE),sd(RMSE))
}

n <- c(100)
set.seed(1)
f<-sapply(n, myRMSE)
f




# Question 6
set.seed(1)
n <- 1000

#Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)

dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))


cor(dat)

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

# Train linear model
fit_1 <- lm(y ~ x_1, data = train_set)
fit_2 <- lm(y ~ x_2, data = train_set)
fit_12 <- lm(y ~ x_1 + x_2, data = train_set)

# Loss Functions
y_hat_1 <- predict(fit_1, test_set)
RMSE_1 <- sqrt(mean((y_hat_1 - test_set$y)^2))

y_hat_2 <- predict(fit_2, test_set)
RMSE_2 <- sqrt(mean((y_hat_2 - test_set$y)^2))

y_hat_12 <- predict(fit_12, test_set)
RMSE_12 <- sqrt(mean((y_hat_12 - test_set$y)^2))  

RMSE_1
RMSE_2
RMSE_12
