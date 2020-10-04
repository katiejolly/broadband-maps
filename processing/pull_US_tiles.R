library(sf)
library(tigris)
library(tidyverse)
library(ooklaverse)
options(tigris_class = "sf")


conn_ci <- connect_dsn("Ookla Redshift CI")
tbl_rs_ci <- tbl_db(conn_ci)
tbl_rs_sql_ci <- tbl_db_sql(conn_ci)


tiles_world <- read_file(here("queries/pull_fixed_tiles.sql")) %>%
  tbl_rs_sql_ci() %>% 
  collect()

tiles_sf <- tiles_world %>%
  st_as_sf(wkt = "tile", crs = 4236) %>%
  st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") # albers


usa <- nation() %>%
  st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

tiles_usa <- st_join(tiles_sf, usa, left = FALSE)

st_write(tiles_usa, "/Users/katie/ookla-for-good/katie-oodi-app/data/tiles_usa_q2_2020.gpkg")
