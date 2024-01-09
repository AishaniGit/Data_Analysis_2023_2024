library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)

Mutator
#urbancalifocusgraph2 <- urbancalifocusgraph2 %>%
#  mutate(Vehicle = Vehicle / 1000)


urbancalifocusgraph2_NOTOT <- filter(urbancalifocusgraph2, Type != "Total")

# Plotting graphs configured by external script
urbancalifocusgraph1 %>% 
  ggplot(aes(x = `Year`, y = `Vehicle`, color = `Type`)) + 
  geom_line() +
  labs(title = "# of Vehicles vs. Time for Different Fuel Types - Urban",
       subtitle = "",
       x = "Years",
       y = "# of Vehicles (thousands)",
       color = "Fuel Type") +
  geom_point() + 
  geom_line(size = 0.0002) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

urbancalifocusgraph2_NOTOT %>% 
  ggplot(aes(x = `Year`, y = `Vehicle`, color = `Type`)) + 
  geom_line() +
  labs(title = "# of Vehicles vs. Time for Different Fuel Types (minus Gasoline) - Urban",
       subtitle = "",
       x = "Years",
       y = "# of Vehicles (thousands)",
       color = "Fuel Type") +
  geom_point() + 
  geom_line(size = 0.0002) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ruralcalifocusgraph1 %>% 
  ggplot(aes(x = `year`, y = `sum_value`, color = `Fuel`)) + 
  geom_line() +
  labs(title = "# of Vehicles vs. Time for Different Fuel Types - Rural",
       subtitle = "",
       x = "Years",
       y = "# of Vehicles (thousands)",
       color = "Fuel Type") +
  geom_point() + 
  geom_line(size = 0.0002) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ruralcalifocusgraph2 %>% 
  ggplot(aes(x = `year`, y = `sum_value`, color = `Fuel`)) + 
  geom_line() +
  labs(title = "# of Vehicles vs. Time for Different Fuel Types (minus Gasoline) - Rural",
       subtitle = "",
       x = "Years",
       y = "# of Vehicles (thousands)",
       color = "Fuel Type") +
  geom_point() + 
  geom_line(size = 0.0002) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))



