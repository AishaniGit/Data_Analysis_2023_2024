library(tidyverse)
library(RColorBrewer)
library(plyr)

#Finding the different types of electric vehicles
unique(washPop$`Electric Vehicle Type`)

#Finding the different counties in Washington
unique(washPop$County)

#creating data frame for rural and urban counties
washPopUrban <- washPop %>%
  filter((County == "Whatcom" |
            County == "Snohomish" |
            County == "Kitsap" |
            County == "King" |
            County == "Pierce"|
            County == "Thurston" |
            County == "Spokane" |
            County == "Benton" |
            County == "Clark"))
washPopUrban <- washPopUrban %>%
  arrange(County)
View(washPopUrban)

WashPopRural <- washPop %>%
  filter((County == "Skamania" |
            County == "Columbia" |
            County == "Lincoln" |
            County == "Ferry" |
            County == "Garfield" |
            County == "Okanogan" |
            County == "Pend Oreille" |
            County == "Adams" |
            County == "Klickitat"))
WashPopRural <- WashPopRural %>%
  arrange(County)
View(WashPopRural)

#Finding amount of cars whose electric range hasn't been researched
washPopUrban %>%
  count(`Clean Alternative Fuel Vehicle (CAFV) Eligibility`)
washPopUrban %>%
  count(`Electric Range`)

WashPopRural %>%
  count(`Clean Alternative Fuel Vehicle (CAFV) Eligibility`)
WashPopRural %>%
  count(`Electric Range`)

#Filtering the data needed for the graphs
washPopUrban1 <- washPopUrban %>%
  filter(`Electric Range` >= 30)
washPopRural1 <- WashPopRural %>%
  filter((`Electric Range` >= 30))

#calculate the mean and median mileage for rural and urban
mean(washPopRural1$`Electric Range`)
median(washPopRural1$`Electric Range`)

mean(washPopUrban1$`Electric Range`)
median(washPopUrban1$`Electric Range`)

#creating histogram for mileage of electric cars
ggplot(washPopUrban1, aes(x = `Electric Range`, fill = County)) + 
  geom_histogram(bins = 50) +
  geom_vline(aes(xintercept=mean(`Electric Range`)), linetype = "dashed", size = 1) +
  theme_classic() +
  scale_fill_brewer(palette = "PuBu") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept=median(`Electric Range`)), linetype = "dotted", size = 1) +
  labs(x = "Electric Range (miles)", y = "Number of Vehicles", title = "Electric Vehicles in Urban Counties that are Clean Alternative Fuel Eligible")

ggplot(washPopRural1, aes(x = `Electric Range`, fill = County)) + 
  geom_histogram(bins = 50) +
  geom_vline(aes(xintercept=mean(`Electric Range`)), linetype = "dashed", size = 1) +
  theme_classic() +
  scale_fill_brewer(palette = "BuPu") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept=median(`Electric Range`)), linetype = "dotted", size = 1) +
  labs(x = "Electric Range (miles)", y = "Number of Vehicles", title = "Electric Vehicles in Rural Counties that are Clean Alternative Fuel Eligible")


