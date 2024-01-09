library(tidyverse)
library(ggmap)
library(htmltools)
library(ggplot2)
library(maps)
library(mapproj)
library(mapdata)
library(tigris)
library(RColorBrewer)

# Manipulation of data points from well known formatting
wkt_data <- data.frame(
  geometry = connecticut$`New Georeferenced Column`,
  label = connecticut$`Station Name`
)


wkt_data <- wkt_data %>%
  mutate(
    lon = as.numeric(str_extract(geometry, "-?\\d+\\.\\d+")),
    lat = as.numeric(str_extract_all(geometry, "-?\\d+\\.\\d+") %>% sapply(`[[`, 2))
  )

#View(wkt_data)

ct_counties <- counties(state = "CT", cb = TRUE, year = 2019)

# Manually inputted population data for CT counties
population_data <- data.frame(
  county = c("Fairfield", "Hartford", "Litchfield", "Middlesex", "New Haven", "New London", "Tolland", "Windham"),
  population = c(945348, 893566, 180333, 162436, 857183, 265206, 150721, 118428)
)

ct_counties <- left_join(ct_counties, population_data, by = c("NAME" = "county"))

# Plot map
ggplot(ct_counties) +
  geom_sf(aes(fill = population.y)) +
  scale_fill_viridis_c() +
  scale_fill_gradient(name = "Population", low = "darkslateblue", high = "lightblue", labels = scales::comma) +
  geom_point(data = wkt_data, aes(x = lon, y = lat), color = "red", size = 1) +
  coord_sf() +  
  theme(aspect.ratio = 1) + 
  theme_minimal() +
  ggtitle("Distribution of Power Stations in Connecticut") +
  labs(x = "longitude",
       y = "latitude")

