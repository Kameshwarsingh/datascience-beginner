library(HistData)
library(caret)
library(dslabs)
library(dplyr)
library(lubridate)

glimpse(GaltonFamilies)
# names(GaltonFamilies)
# head(GaltonFamilies,20)

# GaltonFamilies %>%
#   filter(gender == "male") %>%
#   group_by(family) %>%
#   summarize(mean_size = mean(father, na.rm = TRUE))
  

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father,childHeight) %>%
  rename(son = childHeight)

head(galton_heights)

galton_heights %>% 
  summarize(mean(father), sd(father), mean(son), sd(son))

#correlation between father and son height
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)
#galton_heights %>% summarize(r = cor(mother, son)) %>% pull(r)

y <- galton_heights$son

set.seed(1983)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)


m <- mean(train_set$son)
print(m)
# squared loss
mean((m-test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)


test = c("a","b","c")
print(test)
class(test)

test = factor(test)
print(test)
class(test)


# ##regression line
# mu_x <- mean(galton_heights$father)
# mu_y <- mean(galton_heights$son)
# s_x <- sd(galton_heights$father)
# s_y <- sd(galton_heights$son)
# r <- cor(galton_heights$father, galton_heights$son)
# 
# galton_heights %>% 
#   ggplot(aes(father, son)) + 
#   geom_point(alpha = 0.5) +
#   geom_abline(slope = r * s_y/s_x, intercept = mu_y - r * s_y/s_x * mu_x) 


###predict function - takes a)fitted objects from lm/glm b) Data frame - and predicts the outcome
## predict does similiar work as - using regression line
# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
#loss
mean((y_hat - test_set$son)^2)

y_hat <- predict(fit,test_set)
#loss
mean((y_hat - test_set$son)^2)


?predict.lm
?predict.glm
