###Descriptive 
#Frequency
#Central tendencies
#Dispersion/spread/distribution of data
#Correlation - degree of relation

cars <- read.csv(
  file="Cars.csv",
  header=TRUE,
)
head(cars)

#create frequency table
table(cars$Transmission)

#min fuel
min(cars$Fuel.Economy)

#max
max(cars$Fuel.Economy)


#mean
mean(cars$Fuel.Economy)

#median
median(cars$Fuel.Economy)


quantile(cars$Fuel.Economy)

sd(cars$Fuel.Economy)

cor (
  x=cars$Cylinders,
  y=cars$Fuel.Economy
)

summary(cars)
