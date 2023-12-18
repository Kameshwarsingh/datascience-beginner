a<-10
print(a)

#list workspace
ls()

getwd()
setwd("D:/software/R-workspace/R_Project_week1")


library(dslabs)
data(murders)
class(murders)

str(murders)

#list all states
murders$state

head(murders)

#col names
names(murders)

pop <- murders$population
length(pop)

class(pop)

class(murders$state)

#factor - Factors are useful for storing categorical datamurders[25, 1]
class(murders$region)

levels(murders$region)

#You can also use single square brackets ([) to access rows and columns of a data frame:
data("murders")
murders[2:3,]

#vector
codes <- c(380, 124, 818)
class(codes)
str(codes)


codes <- c(italy = 380, canada = 124, egypt = 818)
codes

seq(1, 10)


x <- c("1", "b", "3")
as.numeric(x)


