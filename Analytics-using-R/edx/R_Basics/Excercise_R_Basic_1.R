install.packages("dslabs")


#Q1
x <- c(2, 43, 27, 96, 18)

sort(x)
order(x)
rank(x)

#Q2
min(x)
which.min(x)

max(x)
which.max(x)


#Q3
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

names(distance) <- name
distance

names(time) <- name
time

v <- cbind(time,distance)
str(v)
df<- data.frame(v)
str(df)
print(df)

time_in_hr <- c(df$time / 60)
time_in_hr
df$time_in_hr = time_in_hr
df


speed_miles_pr_hr <- c(df$distance/df$time_in_hr)
speed_miles_pr_hr
df$speed_miles_pr_hr = speed_miles_pr_hr
df


#Q4


#########################Assessment3

library(dslabs)
data(heights)
options(digits = 3)

str(heights)
mean(heights$height)

ind <- ( heights$height > mean(heights$height))
ind
sum(ind)

levels(heights$sex)

ind_female <- ( heights$sex=="Female")
ind_q2 <- ind & ind_female
sum(ind_q2)

mean(ind_female)

min(heights$height)
?match
match(min(heights$height),heights$height)

heights$sex[1032]

