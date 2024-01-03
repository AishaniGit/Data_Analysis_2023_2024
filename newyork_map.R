library(tidyverse)
library(ggmap)
library(htmltools)
library(ggplot2)
library(maps)
library(mapproj)
library(mapdata)
library(tigris)
library(RColorBrewer)

# NewYork_DemographicsByCounty_sample$Geography <- gsub(" County", "", NewYork_DemographicsByCounty_sample$Geography)
# NewYork_DemographicsByCounty_sample <- NewYork_DemographicsByCounty_sample %>%
#   slice(2:n())
#View(wkt_data)

ny_counties <- tigris::counties(state = "NY", cb = TRUE, year = 2019)

ny_population_data <- data.frame(
  county = (na.omit(NewYork_DemographicsByCounty_sample$Geography)),
  population = as.numeric(na.omit(NewYork_DemographicsByCounty_sample$`...6`))
)

ny_counties <- left_join(ny_counties, ny_population_data, by = c("NAME" = "county"))

# Plot the map
ggplot(ny_counties) +
  geom_sf(aes(fill = `population.y`)) +
  scale_fill_viridis_c() +
  scale_fill_gradient(name = "Population", low = "darkslateblue", high = "lightblue", labels = scales::comma) +
  geom_point(data = nyc, aes(x = `LONGITUDE`, y = `LATITUDE`), color = "red", size = 1) +
  theme_minimal() +
  coord_sf(xlim = c(-73.7,-74.3), ylim = c(40.46,40.93)) +
  ggtitle("New York County Map")

