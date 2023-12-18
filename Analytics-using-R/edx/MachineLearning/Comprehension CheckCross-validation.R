library(caret)
library(tidyverse)


#set.seed(1996) #if you are using R 3.5 or earlier

set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000

x <- matrix(rnorm(n*p), n, p)
dim(x)

colnames(x) <- paste("x", 1:ncol(x), sep = "_")

y <- rbinom(n, 1, 0.5) %>% factor()
summary(y)
x_subset <- x[ ,sample(p, 100)]
dim(x_subset)

fit <- train(x_subset, y, method = "glm")
fit$results


#install.packages("BiocManager")
#BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

pvals <- tt$p.value
pvals

nrow(tt$statistic)
