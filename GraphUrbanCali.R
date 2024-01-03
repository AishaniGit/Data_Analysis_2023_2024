library(tidyverse)

#adds another row that shows the total number of electric cars
CaliTotalData <- CaliTotalData %>%
  mutate(`Total Electric Cars` = (`Battery Electric`+`Plug in Hybrid`+`Hybrid gasoline`+ `Hydrogen fuel cell`)/1000)
CaliTotalData <- CaliTotalData %>%
  arrange(Year)
View(CaliTotalData)

#Makes two data frames one for light and heavy
CaliLightData <- CaliTotalData %>%
  filter(Duty == "Light")
View(CaliLightData)

CaliHeavyData <- CaliTotalData %>%
  filter(Duty == "Heavy")
View(CaliHeavyData)

#creates graphs for light and heavy cars of urban counties
ggplot( CaliLightData, aes(x = Year, y = `Total Electric Cars`, color = County)) + 
  geom_line(aes(color = County, linetype = County)) + 
  labs(x = "Years", y = "Total Electric Vehicles (Thousands)", title = "Light Electric Vehicles in Urban California Counties") +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  geom_line(size = 1)+
  geom_point() +
  theme_classic() +
  scale_color_brewer(palette = "PuBu") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot( CaliHeavyData, aes(x = Year, y = `Total Electric Cars`, color = County)) + 
  geom_line(aes(color = County, linetype = County)) + 
  labs(x = "Years", y = "Total Electric Vehicles (Thousands)", title = "Heavy Electric Vehicles in Urban California Counties") +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  geom_line(size = 1)+
  geom_point() +
  theme_classic() +
  scale_color_brewer(palette = "PuBu") +
  theme(plot.title = element_text(hjust = 0.5))
