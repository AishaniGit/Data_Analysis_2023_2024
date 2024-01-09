library(tidyverse)
library(data.table)
library(dplyr)

print("2018 Data")

cali2018 %>% 
  group_by(`Fuel`) %>% 
  summarise(sum(`Vehicles`))

print("2020 Data")

cali2020 %>% 
  group_by(`Fuel`) %>% 
  summarise(sum(`Vehicles`))

print("2021 Data")

cali2021 %>% 
  group_by(`Fuel`) %>% 
  summarise(sum(`Vehicles`))

print("2022 Data")

cali2022 %>% 
  group_by(`Fuel`) %>% 
  summarise(sum(`Vehicles`))


#rural
rural_table <-rbind(cali2018, cali2020, cali2021, cali2022)
rural_table <- rural_table %>% 
  filter(`Zip Code` == 91901 | `Zip Code` == 91903 | `Zip Code` == 92019)
View(rural_table)

ruralcalifocusgraph1_data <- rural_table %>%
  group_by(`Fuel`, `year`) %>%
  summarise(sum_value = sum(`Vehicles`))

# Convert the result to a data table
ruralcalifocusgraph1 <- as.data.table(ruralcalifocusgraph1_data)
ruralcalifocusgraph2 <- filter(ruralcalifocusgraph1, Fuel != "Gasoline")


