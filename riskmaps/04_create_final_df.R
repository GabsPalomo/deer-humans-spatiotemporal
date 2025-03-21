# Create a data frame with all the covariates extracted by each hexagon cell 
# Not the most elegant way. I need to go back to the original files and fix the 
# rds files so I don't export them as lists but as data frames. 

library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(sf)

# Bring all cities together into one single df 
path_rst <- list.files('./rasters/final_scaled_urban_covariates/', full.names = TRUE)
path_rst_names <- list.files('./rasters/final_scaled_urban_covariates/', full.names = FALSE)
# Extract only the first part of the string before the underscore _
path_rst_names <-  stringr::str_extract(path_rst_names, "^[^_]+")
lc_list <- lapply(path_rst, readr::read_rds)
names(lc_list) <- path_rst_names

# Extract popdens as a data frame 
pop_dens <- lc_list["popdens"]
pop_dens <- do.call(data.frame, pop_dens)
# pop_dens <- st_as_sf(pop_dens)
class(pop_dens)
# st_crs(pop_dens)$proj4string

pop_dens <- pop_dens |> 
  rename(id = popdens.id,
         city_uwin = popdens.city_uwin,
         pop_dens = popdens.pop_dens,
         fcity = popdens.fcity,
         urban_name = popdens.urban_name, 
         type = popdens.type, 
         geometry = popdens.geometry, 
         mean_city_pop = popdens.mean_city_pop, 
         global_sd_pop = popdens.global_sd, 
         sc_log_pop = popdens.sc_log_pop) |> 
  select(id, city_uwin, fcity, urban_name, sc_log_pop, mean_city_pop, global_sd_pop)


# Extract ag as a data.frame
ag <- lc_list["ag"]
ag <- do.call(data.frame, ag)
# ag <- st_as_sf(ag)
class(ag)
# st_crs(ag)$proj4string

ag <- ag |> 
  rename(id = ag.id,
         city_uwin = ag.city_uwin,
         area = ag.ag_area, 
         prop = ag.ag_prop,
         fcity = ag.fcity,
         urban_name = ag.urban_name, 
         type = ag.type, 
         geometry = ag.geometry, 
         mean_city_ag = ag.mean_city_ag_value, 
         global_sd_ag = ag.global_sd, 
         scale_ag = ag.scale_ag_value) |> 
  select(id, city_uwin, fcity, urban_name, scale_ag, mean_city_ag, global_sd_ag)

# Extract habitat as a data frame 
habitat <- lc_list["habitat"]
habitat <- do.call(data.frame, habitat)
# habitat <- st_as_sf(habitat)
class(habitat)
# st_crs(habitat)$proj4string

habitat <- habitat |> 
  rename(id = habitat.id,
         city_uwin = habitat.city_uwin,
         area = habitat.habitat_area, 
         prop = habitat.habitat_prop,
         fcity = habitat.fcity,
         urban_name = habitat.urban_name, 
         type = habitat.type, 
         geometry = habitat.geometry, 
         mean_city_habitat = habitat.mean_city_habitat_value, 
         global_sd_habitat = habitat.global_sd, 
         scale_habitat = habitat.scale_habitat_value) |> 
  select(id, city_uwin, fcity, urban_name, scale_habitat, mean_city_habitat, global_sd_habitat)

# Extract impervious as a data frame 
impervious <- lc_list["impervious"]
impervious <- do.call(data.frame, impervious)
# impervious <- st_as_sf(impervious)
class(impervious)
# st_crs(ag)$proj4string

impervious <- impervious |> 
  rename(id = impervious.id,
         city_uwin = impervious.city_uwin,
         fcity = impervious.fcity,
         urban_name = impervious.urban_name, 
         type = impervious.type, 
         geometry = impervious.geometry, 
         mean_city_impervious = impervious.mean_city_imp_value, 
         global_sd_impervious = impervious.global_sd, 
         scale_impervious = impervious.scale_imp_value) |> 
  select(id, city_uwin, fcity, urban_name, scale_impervious, mean_city_impervious, global_sd_impervious)

#  Combine all into one dataframe 
final_covariates <- left_join(ag, habitat)
final_covariates <- left_join(final_covariates, impervious)
final_covariates <- left_join(final_covariates, pop_dens)                              

names(final_covariates)                              

readr::write_csv(final_covariates, './covariates/final_urban_covariates.csv')

# Make a dataframe with only the scaled covariates 
final_scaled_covariates <- final_covariates |> 
  select(id, city_uwin, scale_ag, scale_habitat, scale_impervious, sc_log_pop)

readr::write_csv(final_scaled_covariates, './covariates/final_urban_scaled_covariates.csv')

# Make the final covariates a shapefile 
hex_grid <- sf::st_read('./shapefiles/hex_grid_urban.shp') 
st_crs(hex_grid)$proj4string

final_covariates_shp <- left_join(final_covariates, hex_grid)
class(final_covariates_shp)
final_covariates_shp <- st_as_sf(final_covariates_shp)

write_sf(final_covariates_shp, './shapefiles/final_covariates.shp')
