library(tidyverse)
library(RColorBrewer)
library(ggppmisc)

#creating Model year column as numeric
USEPA <- USEPA %>%
  filter(`Model Year` <= 2021)
USEPA <- transform(USEPA, `Model Year` = as.numeric(`Model Year`))

#creating graphs of real world co2 city and hwy for truck and car 
my_colors <- RColorBrewer::brewer.pal(9, "PuBuGn")[4:8]
ggplot(USEPA, aes(x = `Model.Year`, y = `Real.World.CO2_City..g.mi.`, color = `Regulatory.Class`)) +
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
ggplot(USEPA, aes(x = `Model.Year`, y = `Real.World.CO2_Hwy..g.mi.`, color = `Regulatory.Class`)) +
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
