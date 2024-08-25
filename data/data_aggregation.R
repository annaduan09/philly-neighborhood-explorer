#### Data Aggregation - Tract, ZCTA, Neighborhood ####

##### Libraries & Configs #####
library(sf)
library(tigris)
library(tidyverse)
library(conflicted)
library(mapview)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

##### Neighborhood level #####
### Geometry
nb_bounds <- st_read("data/neighborhood/phl_neighs_2024.geojson") %>%
  st_make_valid() %>%
  st_transform(crs = "EPSG:4326") %>%
  select(neighborhood = MAPNAME) 

# ### Features
# clusters <- st_read("data/neighborhood/clusters.geojson") %>%
#   st_make_valid() %>%
#   st_transform(crs = "EPSG:4326") %>%
#   st_drop_geometry() %>%
#   rename(neighborhood = nb_name)

### Final
dat_nb <- nb_bounds 

##### ZCTA level #####
### Geometry
zcta_bounds <- zctas(year = "2022") %>%
  filter(substr(GEOID20, 1, 3) == "191") %>%
  st_transform(crs = "EPSG:4326") %>%
  select(zip_code = GEOID20)

safmr <- st_read("data/zcta/safmr_groups_2023.csv")

dat_zcta <- zcta_bounds %>%
  left_join(safmr, by = "zip_code") %>%
  st_as_sf()

##### Tract level #####
### Geometry
tract_bounds <- tracts(state = "PA", county = "Philadelphia") %>%
  st_transform(crs = "EPSG:4326") %>%
  select(tract = GEOID)

### Features
acs <- st_read("data/tract/acs_2022.csv") %>%
  rename(tract = GEOID) 

amenities <- st_read("data/tract/amenities_tract.geojson") %>%
  pivot_wider(id_cols = c("nb_name", "geometry"), names_from = type, values_from = count_per_mi2) %>%
  st_transform(crs = "EPSG:4326") %>%
  st_centroid()

### Final
dat_tract <- st_intersection(amenities, tract_bounds) %>%
  st_drop_geometry() %>%
  right_join(tract_bounds, by = "tract") %>%
  left_join(acs, by = "tract") %>%
  st_as_sf() %>%
  rename(tract_neigh = nb_name)
  
##### Export #####
bootleg_crosswalk <- tract_bounds %>%
  st_centroid() %>%
  st_intersection(zcta_bounds) %>%
  st_intersection(nb_bounds) %>%
  st_drop_geometry()

tract_panel.sf <- dat_tract %>%
  left_join(bootleg_crosswalk, by = "tract") %>%
  left_join(st_drop_geometry(dat_nb), by = "neighborhood") %>%
  left_join(st_drop_geometry(dat_zcta), by = "zip_code")

write_sf(tract_panel.sf, "data/tract_panel.geojson", driver = "geojson")
