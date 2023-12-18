library(dslabs)
library(caret)
library(tidyverse)


balls <- rep(c("cyan", "magenta", "yellow"), times = c(3,5,7))   
balls
sample(balls, 1)

#?sample
#sample_n(tbl, size, replace = FALSE, weight = NULL, .env = NULL, ...)


##sample it for 10K times
B <- 10000
events <- replicate(B, sample(balls, 1))
tab <- table(events)
tab
prop.table(tab)

#Q1
index <- (events == "cyan")
sum(index)
mean(index)



#Q2 not cyan
index <- (events != "cyan")
sum(index)
mean(index)

#Q3 - sample 2 at a time, without replacement
B <- 100000
events <- replicate(B, sample(balls, 2, replace=FALSE))
#events
#class(events)
draw_1 <- events[1,]
draw_2 <- events[2,]

#draw_1
#draw_2

index_1 <- (draw_1 == "cyan")
index_2 <- (draw_2 != "cyan")
index <- (index_1 & index_2)
mean(index)




#Q4 - sample 2 at a time, without replacement
B <- 100000
events <- replicate(B, sample(balls, 2, replace=TRUE))
#events
#class(events)
#Row_1 : First draw; Row_2 : Second draw
draw_1 <- events[1,]
draw_2 <- events[2,]

#draw_1
#draw_2

index_1 <- (draw_1 == "cyan")
index_2 <- (draw_2 != "cyan")
index <- (index_1 & index_2)
mean(index)


