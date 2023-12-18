getwd()
cars <- read.table(
                file="Cars.txt",
                header=TRUE,
                sep="\t",
                quote="\""
                )
head(cars)

library(dplyr)
temp <- select (
        .data = cars,
        Transmission,
        Cylinders,
        Fuel.Economy
)
head(temp)

#filter
temp <- filter(
  .data=temp,
  Transmission=="Automatic"
)
head(temp)

#compute a new column
temp <- mutate (
  .data =temp,
  Consumption = Fuel.Economy * 0.425
)
head(temp)

#Grouped data frame by a column
temp <- group_by (
  .data=temp,
  Cylinders
)
head(temp)
nrow(temp)

temp <- summarize( 
  .data = temp,
  Avg.Consumption = mean(Consumption)
  )

# Inspect the results
head(temp)
nrow(temp)

#convert to data frame
efficiency <- as.data.frame(temp)
print(efficiency)


# Chain methods together
efficiency <- cars %>%
  select(Fuel.Economy, Cylinders, Transmission) %>%
  filter(Transmission == "Automatic") %>%
  mutate(Consumption = Fuel.Economy * 0.425) %>%
  group_by(Cylinders) %>%
  summarize(Avg.Consumption = mean(Consumption)) %>%
  arrange(desc(Avg.Consumption)) %>%
  as.data.frame()

# Inspect the results
print(efficiency)

# Save the results to a CSV file
write.csv(
  x = efficiency,
  file = "Fuel Efficiency.csv",
  row.names = FALSE)

