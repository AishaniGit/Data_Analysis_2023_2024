library(tidyverse)
library(RColorBrewer)

#setting the range for the data
washReg1 <- washReg %>%
  filter(`Odometer Reading` > 0)

#creating histogram for type of Clean Alternative Fuel Type and their mileage
ggplot(washReg1, aes(x = `Odometer Reading`, fill = `Clean Alternative Fuel Vehicle Type`)) + 
  geom_histogram(bins = 50, color = "white") +
  geom_vline(aes(xintercept=mean(`Odometer Reading`)), linetype = "dashed", size = 1) +
  theme_classic() +
  scale_fill_brewer(palette = "PuBu") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  xlim(0,150000) +
  ylim(0,5000) +
  labs(x = "Distance Traveled (miles)", y = "Number of Vehicles", title = "Distance Traveled of Different Clean Alternative Fuel Vehicles")

#finding the mean and median values for the odometer reading
mean(washReg1$`Odometer Reading`)
median(washReg1$`Odometer Reading`)
