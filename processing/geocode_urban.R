library(sf)
library(tigris)
library(tidyverse)
options(tigris_class = "sf")

tiles_usa <- st_read("/Users/katie/ookla-for-good/katie-oodi-app/data/tiles_usa_q2_2020.gpkg")

urban <- urban_areas() %>%
  st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")


tiles_all_urban_areas <- tiles_usa %>%
  st_join(urban %>%
            dplyr::select(GEOID10, NAME10), left = FALSE)

st_write(tiles_all_urban_areas, "/Users/katie/ookla-for-good/katie-oodi-app/data/tiles_all_urban_areas.gpkg")

tiles_all_urban_areas <- st_read("/Users/katie/ookla-for-good/katie-oodi-app/data/tiles_all_urban_areas.gpkg")

tiles_all_urban_areas_clean <- tiles_all_urban_areas %>% 
  mutate(tile = st_as_text(geom)) %>%
  st_set_geometry(NULL) %>%
  dplyr::select(quadkey, 
                avg_d_kbps, 
                avg_u_kbps, avg_lat_ms, tests, devices, geoid = GEOID10, name = NAME10, tile)

write_csv(tiles_all_urban_areas_clean, "/Users/katie/ookla-for-good/katie-oodi-app/app/app-data/urban_area_tiles.csv")
