library(HistData)
library(caret)
library(dslabs)
library(dplyr)
library(lubridate)
library(dslabs)

###
#Y=1 => Female
#Y=0 => Male
#p(x)=Pr(Y=1|X=x)=β0+β1x
###

data("heights")
y <- heights$height

set.seed(2) #if you are using R 3.5 or earlier
#set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)


#filter : height==66, 
#what is probability of gender being "Female" (mean)
train_set %>% 
  filter(round(height)==66) %>%
  count((sex=="Female"))
## total 40
## 32 - Male
## 8 Female
## P = 8/40 = 1/5 = 0.2


train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))

# ?n() - number of observations
## Find probability of being Femal for given height (if number of obsevations for the given height is more than 10)
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female"))

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

#train using linear regression
#?as.numeric  - convert to Integer
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% 
          lm(y ~ height, data = .)
summary(lm_fit)

#predict on test set
p_hat <- predict(lm_fit, test_set)
confusion_matrix=confusionMatrix(p_hat, test_set$sex)
confusion_matrix$overall["Accuracy"]
confusion_matrix

#use probability 
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusion_matrix=confusionMatrix(y_hat, test_set$sex)
confusion_matrix$overall["Accuracy"]
confusion_matrix

