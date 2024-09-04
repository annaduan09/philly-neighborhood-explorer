#### Data Aggregation - Tract, ZCTA, Neighborhood ####

##### Libraries & Configs #####
library(sf)
library(tigris)
library(tidyverse)
library(conflicted)
library(mapview)
library(scales)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

##### Point - no aggregation #####
landmarks <- st_read("data/point/landmarks.geojson") %>%
  st_transform(crs = "EPSG:4326") %>%
  group_by(PARENT_NAME) %>%
  slice_head(n = 1) %>%
  select(NAME) %>%
  st_centroid() %>%
  # Get lng and lat
  mutate(lng = st_coordinates(geometry)[, 1],
         lat = st_coordinates(geometry)[, 2])

# write_sf(landmarks, "data/landmarks.geojson")

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

vouchers <- st_read("data/tract/vouchers_tract_2023.csv") %>%
  mutate(count = 1) %>%
  group_by(GEOID) %>%
  summarise(vouchers = sum(count)) %>%
  select(tract = GEOID, vouchers) %>%
  filter(tract %in% tract_bounds$tract)

# violent <- c('100', '200', '300', '400', '800', '1700', '2000', '1500')
# crime <- st_read("/Users/annaduan/Desktop/GitHub/philly-neighborhood-explorer/data/point/crime\ 2024/incidents_part1_part2.shp") %>%
#   st_transform(crs = "EPSG:4326") %>%
#   filter(!is.na(point_x) & ucr_genera %in% violent) %>%
#   select(geometry) %>%
#   st_intersection(tract_bounds) %>%
#   group_by(tract) %>%
#   summarise(crime_incidents = n())

shootings <- st_read("data/point/shootings.geojson") %>%
  st_transform(crs = "EPSG:4326") %>%
  select(geometry) %>%
  st_intersection(tract_bounds) %>%
  group_by(tract) %>%
  summarise(shootings = n()) %>%
  st_drop_geometry() 
  
st_write(shootings, "data/tract/shootings_tract.geojson", driver = "GeoJSON")

### Final
dat_tract <- st_intersection(amenities, tract_bounds) %>%
  st_drop_geometry() %>%
  left_join(shootings, by = "tract") %>%
  left_join(acs, by = "tract") %>%
  left_join(vouchers, by = "tract") %>%
  left_join(tract_bounds, by = "tract") %>%
  st_as_sf() %>%
  rename(tract_neigh = nb_name)
  
##### Merge #####
bootleg_crosswalk <- tract_bounds %>%
  st_centroid() %>%
  st_intersection(zcta_bounds) %>%
  st_intersection(nb_bounds) %>%
  st_drop_geometry()

dat_merge <- dat_tract %>%
  left_join(bootleg_crosswalk, by = "tract") %>%
  left_join(st_drop_geometry(dat_nb), by = "neighborhood") %>%
  left_join(st_drop_geometry(dat_zcta), by = "zip_code")

##### Scale #####
to_scale <- c("entertainment", "kids", "nightlife", "restaurant", "beauty", "grocery", "shopping", "arts",                             
              "education", "historic", "parks", "healthcare", "population2022", "median_age2022", "child_male2022",                    
              "child_female2022", "child_pct2022", "female_pct2022",                    
              "white_pct2022", "black_pct2022", "hispanic_pct2022",                  
              "female_hh_pct2022", "public_asst_pct2022", "poverty_pct2022",                   
              "medhhinc2022", "renter_pct2022", "vacancy_pct2022",                   
              "same_house_pct2022", "same_county_move_pct2022", "dif_county_same_state_move_pct2022",
              "dif_state_move_pct2022", "abroad_move_pct2022", "owner_housing_condition_pct2022",   
              "renter_housing_condition_pct2022", "unemployed_pct2022", "race_ice2022",                      
              "vouchers", "shootings_100k")

dat_scale <- dat_merge %>%
  mutate(shootings_100k = ifelse(shootings != 0 & !is.na(shootings) & !is.na(as.numeric(population2022)) & as.numeric(population2022) != 0, 10000*shootings/as.numeric(population2022), 0),
         across(all_of(to_scale), ~ as.numeric(.))) %>%
  mutate(across(all_of(to_scale), ~ rescale(.x, to = c(0, 1)))) %>%
  mutate(across(c(poverty_pct2022, vacancy_pct2022, shootings_100k), ~ 1 - .x)) #flip undesirable scores

dat_final <- dat_scale %>%
  # replace NAs with 0
  mutate(across(all_of(to_scale), ~ replace_na(.x, 0))) 
  
##### Export #####
write_sf(dat_final, "data/panel.geojson", driver = "geojson")
