library(dslabs)
library(caret)
library(tidyverse)


data("heights")
glimpse(heights)

# heights <- mutate_if(heights, is.character, as.factor)
# glimpse(heights)

y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Estimating averages and standard deviations
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# Estimating the prevalence
prevalence <- train_set %>% 
      summarize(prevalence=mean(sex=="Female")) %>% 
      pull(prevalence)

prevalence

# Getting an actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*prevalence / (f1*prevalence + f0*(1 - prevalence))


?qplot
p_hat_bayes
#x_range <- seq(from = 0.1, to = 1, length.out = 10)

qplot(p_hat_bayes,
      geom="histogram",
      binwidth = 0.05,
      xlim=c(0.0,1.0)
)

# library(ggplot2)
# p_hat_bayes_df<- as.data.frame(p_hat_bayes, stringsAsFactors=FALSE)
# glimpse(p_hat_bayes_df)
p_hat_bayes



# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Changing the cutoff of the decision rule
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

# Draw plot
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)

