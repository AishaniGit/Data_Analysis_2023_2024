library(tidyverse)

#Makes new row to show total number of electric vehicles
CaliRuralData <- CaliRuralData %>%
  mutate(`Total Electric Cars` = (`Battery Electric` + `Hydrogren Fuel` + `Hyrid Gasoline` + `Plug-in Hybrid`)/1000)
View(CaliRuralData)

#Creates two data frames for light and heavy
CaliLightR <- CaliRuralData %>%
  filter(Duty == "Light")
View(CaliLightR)

CaliHeavyR <- CaliRuralData %>%
  filter(Duty == "Heavy")
View(CaliHeavyR)

#Creates plots for the two data frames
ggplot(CaliLightR, aes(x = Year, y = `Total Electric Cars`, color = County)) + 
  geom_line(aes(color = County, linetype = County)) + 
  labs(x = "Years", y = "Total Electric Vehicles (Thousands)", title = "Light Electric Vehicles in Rural California Counties") +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  geom_line(size = 1)+
  geom_point() +
  theme_classic() +
  scale_color_brewer(palette = "BuPu") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(CaliHeavyR, aes(x = Year, y = `Total Electric Cars`, color = County)) + 
  geom_line(aes(color = County, linetype = County)) + 
  labs(x = "Years", y = "Total Electric Vehicles (Thousands)", title = "Heavy Electric Vehicles in Rural California Counties") +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  geom_line(size = 1)+
  geom_point() +
  theme_classic() +
  scale_color_brewer(palette = "BuPu") +
  theme(plot.title = element_text(hjust = 0.5))
