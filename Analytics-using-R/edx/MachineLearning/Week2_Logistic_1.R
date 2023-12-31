library(HistData)
library(caret)
library(dslabs)
library(dplyr)
library(lubridate)
library(dslabs)


heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat)

# fit logistic regression model
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor

confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]
