library(sf)
library(tigris)
library(tidyverse)
library(ooklaOpenDataR)
options(tigris_class = "sf")

tiles_world <- get_performance_tiles(service = "fixed", year = 2020, quarter = 3, sf = TRUE)

tiles_sf <- tiles_world %>%
  st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") # albers

usa <- nation() %>%
  st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

tiles_usa <- st_join(tiles_sf, usa, left = FALSE)

st_write(tiles_usa, "/Users/katie/ookla-for-good/katie-oodi-app/data/tiles_usa_q3_2020.gpkg")
