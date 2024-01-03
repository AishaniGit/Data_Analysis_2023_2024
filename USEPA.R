library(tidyverse)
library(RColorBrewer)
library(ggppmisc)

#creating Model year column as numeric
USEPA <- USEPA %>%
  filter(`Model Year` <= 2021)
USEPA <- transform(USEPA, `Model Year` = as.numeric(`Model Year`))

#filter by regulatory class
filtered_data_truck<- USEPA %>%
  filter(`Regulatory.Class` == 'Truck')

filtered_data_car<- USEPA %>%
  filter(`Regulatory.Class` == 'Car')

#creating graphs of real world co2 city and hwy for truck and car 
my_colors <- RColorBrewer::brewer.pal(9, "PuBuGn")[4:8]
q <- ggplot(USEPA, aes(x = `Model.Year`, y = `Real.World.CO2_City..g.mi.`, color = `Regulatory.Class`)) +
  geom_point() +
  geom_smooth(aes(fill = Regulatory.Class)) +
  labs(x = "Year", y = "CO2 (g/mi)", title = "CO2 Emitted from Vehicles in Cities") +
  theme_classic() +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  xlim(1975,2030) +
  stat_smooth(method= "lm", se = FALSE, fullrange = TRUE, linetype = "dashed")

my_color <- RColorBrewer::brewer.pal(9, "Purples")[4:9]
p <- ggplot(USEPA, aes(x = `Model.Year`, y = `Real.World.CO2_Hwy..g.mi.`, color = `Regulatory.Class`)) +
  geom_point() +
  geom_smooth(aes(fill = Regulatory.Class)) +
  labs(x = "Year", y = "CO2 (g/mi)", title = "CO2 Emitted from Vehicles on Highways") +
  theme_classic() +
  scale_color_manual(values = my_color) +
  scale_fill_manual(values = my_color) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  xlim(1975,2030) +
  stat_smooth(method= "lm", se = FALSE, fullrange = TRUE, linetype = "dashed")

#prediction points for CO Emitted in Cities for 2023
x <- 2030
y1 <- predict(lm(Real.World.CO2_City..g.mi. ~ Model.Year, data = filtered_data_car), data.frame(Model.Year = x))
y2 <- predict(lm(Real.World.CO2_City..g.mi. ~ Model.Year, data = filtered_data_truck), data.frame(Model.Year = x))

q <- q + geom_text(aes(x = x, y = y1, label = paste("(", x, ", ", round(y1, 2), ")", sep = "")), color = "black", vjust = -0.5)
q <- q + geom_text(aes(x = x, y = y2, label = paste("(", x, ", ", round(y2, 2), ")", sep = "")), color = "black", vjust = -0.5)
q

#prediction points for CO2 Emitted on Highways in 2023
y3 <- predict(lm(Real.World.CO2_Hwy..g.mi. ~ Model.Year, data = filtered_data_car), data.frame(Model.Year = x))
y4 <- predict(lm(Real.World.CO2_Hwy..g.mi. ~ Model.Year, data = filtered_data_truck), data.frame(Model.Year = x))

p <- p + geom_text(aes(x = x, y = y3, label = paste("(", x, ", ", round(y3, 2), ")", sep = "")), color = "black", vjust = -0.5)
p <- p + geom_text(aes(x = x, y = y4, label = paste("(", x, ", ", round(y4, 2), ")", sep = "")), color = "black", vjust = -0.5)
p
  
  

