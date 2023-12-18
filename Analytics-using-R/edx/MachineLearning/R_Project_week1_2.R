library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)


y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
summary(y)
summary(x)


inclass<-dat[dat$type=="inclass",]
students <- data.frame(inclass)
head(students)
summary(students)
summary(inclass)
library( data.table )
setDT( students )[ , 100 * .N / nrow( students ), by =sex  ]


online<-dat[dat$type=="online",]
students <- data.frame(online)
head(students)
summary(students)
summary(online)
library( data.table )
setDT( students )[ , 100 * .N / nrow( students ), by =sex  ]


