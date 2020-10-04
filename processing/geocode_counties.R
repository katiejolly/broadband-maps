library(sf)
library(tigris)
library(tidyverse)
options(tigris_class = "sf")

tiles_usa <- st_read("/Users/katie/ookla-for-good/katie-oodi-app/data/tiles_usa_q2_2020.gpkg")

counties <- counties() %>%
  st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

tiles_all_counties <- tiles_usa %>%
  st_join(counties  %>%
            select(GEOID, NAME, NAMELSAD, STATEFP), left = FALSE)

st_write(tiles_all_counties, "/Users/katie/ookla-for-good/katie-oodi-app/data/tiles_all_counties.gpkg")


tiles_all_counties_clean <- tiles_all_counties %>% 
  mutate(tile = st_as_text(geom)) %>%
  st_set_geometry(NULL) %>%
  left_join(fips_codes %>% select(state_code, county, state), by = c("STATEFP" = "state_code", "NAMELSAD" = "county")) %>%
  mutate(name = paste0(NAMELSAD, ", ", state)) %>%
  dplyr::select(quadkey, 
                avg_d_kbps, 
                avg_u_kbps, 
                avg_lat_ms, 
                tests, 
                devices, 
                geoid = GEOID.y, 
                short_name = NAME.y,
                long_name = NAMELSAD,
                name,
                tile)

write_csv(tiles_all_counties_clean, "/Users/katie/ookla-for-good/katie-oodi-app/app/app-data/county_tiles.csv")