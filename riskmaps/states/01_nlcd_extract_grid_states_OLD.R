## Script to extract NLCD variables into a hexagonal grid for each state 
## NLCD database 

# install FedData package from Github
# devtools::install_github("ropensci/FedData") 
# this plugs into Github directly. Downloading FedData from Cran won't work.
# Fed data from github is v. 4.0.1

library(FedData)
library(sf)
library(dplyr)
library(purrr)
library(terra)
library(raster)
library(exactextractr)
library(rasterVis)
library(purrr)
library(readr)
library(tigris)
library(tmap)

my_states <- states(cb = TRUE) |> 
  filter(NAME %in% c("Illinois", "Indiana", "Iowa", "Arkansas", "Maryland", "District of Columbia", 
                   "Delaware", "Virginia", "Mississippi", "Massachusetts")) 
st_crs(my_states)$proj4string

# old projection
# rp <- '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'
# new projection based on NLCD products
rp <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
my_states_rp <- st_transform(my_states, crs = rp)

tm_shape(my_states_rp)+
  tm_fill(col = 'NAME')

my_states_list <- split(my_states_rp, my_states_rp$NAME)

# Combine VA, MD, and DC 
dc <- my_states_list[["District of Columbia"]]
va <- my_states_list[["Virginia"]]
md <- my_states_list[["Maryland"]]

my_states_list[["dmv"]] <- rbind(dc, va, md)
my_states_list <- purrr::list_modify(my_states_list, "Virginia" = NULL)
my_states_list[["Virginia"]] <- NULL 
my_states_list[["Maryland"]] <- NULL
my_states_list[["District of Columbia"]] <- NULL


# Raster with impervious cover -nullfile()# Raster with impervious cover -------------------------------------------------
imp <- raster("./rasters/nlcd_2021_impervious_l48_20230630/nlcd_2021_impervious_l48_20230630.img")
st_crs(imp)$proj4string
# "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Crop the raster only to the size of the states of interest 
imp_crop <- terra::crop(imp, my_states_rp, mask= T)
writeRaster(imp_crop, filename = "./rasters/imp_crop.tif", format="GTiff", overwrite=TRUE)

tm_shape(imp_crop)+
  tm_raster()


# Crop raster by state and save each individual raster
imp_list <-  vector('list', length = length(my_states_list))
names(imp_list) <- names(my_states_list)

for (i in 1:length(imp_list)){
  imp_list[[i]] <- terra::crop(imp, my_states_list[[i]], mask = T)
  writeRaster(imp_list[[i]], 
              filename = paste0("./rasters/impervious_states/", names(imp_list)[[i]], "imp_crop.tif"), 
              format="GTiff", 
              overwrite=TRUE)
}

# Read rasters by state and load them in environment 
# path_rst <- list.files('./rasters/impervious_states/', full.names = TRUE)
# path_rst_names <- list.files('./rasters/impervious_states/', full.names = FALSE)
# path_rst_names <-  stringr::str_extract(path_rst_names, "^[A-Za-z]+(?=imp)")
# imp_list <- lapply(path_rst, raster)
# names(imp_list) <- path_rst_names


# Reorganize raster list so it matches the city_uwin order 
new_names <- c("Massachusetts", "Illinois",  "Iowa", "Indiana", "Iowa", "Mississippi", "Arkansas", "dmv", "Delaware")
imp_list <- imp_list[c(new_names)]

# Hexagonal grid ---------------------------------------------------------------
hex_grid <- readr::read_rds('./shapefiles/hex_grid_states.rds') 
plot(hex_grid)
st_crs(hex_grid)$proj4string

# Split the df into a list by city 
hex_grid_list <- split(hex_grid, hex_grid$city_uwin)
list_names <- names(hex_grid_list)

# Create a list in which to store the covariate values for each city 
df_list <- vector('list', length = length(hex_grid_list))
names(df_list) <- names(hex_grid_list)  

for (i in 2:length(hex_grid_list)){
  # sites_list_rp[[i]] <- tibble::rowid_to_column(sites_list_rp[[i]], 'ID')
  
  impdf <- raster::extract(x = imp_list[[i]], 
                           y =  hex_grid_list[[i]], 
                           df = TRUE)
  
  impdf <- impdf |> 
    rename(imp_value = ends_with('_crop'))
  
  df_list[[i]] <- impdf |> 
    group_by(ID) |> 
    summarise_at(vars(imp_value), 
                 ~mean(imp_value, na.rm = TRUE))
  
}

# Left join the hexagon list to the impervious values list to have all the hexagons 
df_list <- map2(
  .x = hex_grid_list, 
  .y = df_list, 
  .f = left_join, 
        by = join_by("id" == "ID")
)

class(df_list[[1]])

write_rds(df_list, './rasters/impervious_.rds')

## Scale the impervious cover values by city -----------------------------------
scale_imp_values <- data.frame(city_uwin = c('boma', 'chil', 'deio', 'inin', 'ioio', 'jams', 'lrar', 'naca', 'wide'), 
                               fcity = c('Boston, MA', 'Chicago, IL', 'Des Moines, IA',"Indianapolis, IN", 
                                         'Iowa City, IA', 'Jackson, MS', "Little Rock, AK", 'Washington DC', 
                                         'Wilmington, DE'), 
                                         mean_city_imp_value = c(51.0576809, 58.88994933, 39.55523475, 41.07061489, 37.78523066, 
                                                       38.98231775, 41.51343705, 40.22171956, 38.70830415), 
                                         global_sd = rep(16.6170552983984, 9))

scale_imp_values_list <- split(scale_imp_values, scale_imp_values$city_uwin)

df_list_scaled <- map2(
  .x = df_list, 
  .y = scale_imp_values_list, 
  .f = left_join, by = join_by("city_uwin")
)

df_list_scaled <- map(
  .x = df_list_scaled, 
  .f = ~ mutate(.x, 
                scale_imp_value = (imp_value - mean_city_imp_value)/global_sd) 
)

write_rds(df_list_scaled, './rasters/impervious_scaled.rds')

# readr::write_csv(sites_imp_scale, './covariates/rasters/impervious_allcities_df.csv')

library(tmap)
tmap_mode("view")
test <- df_list_scaled[["naca"]]
tm_shape(test)+
  tm_fill(col = "scale_imp_value")

# Agriculture ------------------------------------------------------------------
# land cover raster of all the us
lc_raster <- raster("./rasters/nlcd_2021_land_cover_l48_20230630/nlcd_2021_land_cover_l48_20230630.img")
st_crs(lc_raster)$proj4string
# "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Crop raster by state and save each individual raster
lc_list <-  vector('list', length = length(my_states_list))
names(lc_list) <- names(my_states_list)

for (i in 1:length(lc_list)){
  lc_list[[i]] <- terra::crop(lc_raster, my_states_list[[i]], mask = T)
  writeRaster(lc_list[[i]], 
              filename = paste0("./rasters/landcover_states/", names(lc_list)[[i]], "_landcover_crop.tif"), 
              format="GTiff", 
              overwrite=TRUE)
}

# Read rasters by state and load them in a list
path_rst <- list.files('./rasters/landcover_states/', full.names = TRUE)
path_rst_names <- list.files('./rasters/landcover_states/', full.names = FALSE)
path_rst_names <-  stringr::str_extract(path_rst_names,  "^[^_]+")
lc_list <- lapply(path_rst, raster)
names(lc_list) <- path_rst_names


# Reorganize raster list so it matches the city_uwin order 
new_names <- c("Massachusetts", "Illinois",  "Iowa", "Indiana", "Iowa", "Mississippi", "Arkansas", "dmv", "Delaware")
lc_list <- lc_list[c(new_names)]

#develop set of rules for reclassifying the raster to indicate which classes should be selected
reclass.rules <- as.data.frame(matrix(c(11, 0, #open water
                                        21, 0, #developed, open space
                                        22, 0, #developed, low intensity
                                        23, 0, #developed, medium intensity
                                        24, 0, #developed, high intensity
                                        31, 0, #barren land
                                        41, 0, #deciduous forest
                                        42, 0, #evergreen forest
                                        43, 0, #mixed forest
                                        52, 0, #shrub/scrub
                                        71, 0, #grassland/herbaceous
                                        81, 1, #pasture/hay
                                        82, 1, #cultivated crops
                                        90, 0, #woody wetlands
                                        95, 0), #emergent herbaceous wetlands
                                      ncol = 2, #column 1 is original cell value, column 2 is new cell value
                                      byrow = TRUE)) #tells R to fill the matrix by column, not by row

# retitle matrix - more to keep things straight than anything else
names(reclass.rules) <- c("ID", "reclass")

## Create a list into which we will add all the covariate values  
ag_list <- vector('list', length = length(hex_grid_list))
names(ag_list) <- names(hex_grid_list)  

# Function to summarise area and proportion of each class in raster for each hexagon
sum_cover <- function(x){
  list(x %>%
         group_by(value) %>%
         summarize(total_area = sum(coverage_area)) %>%
         mutate(proportion = total_area/sum(total_area)))
}

# Let's populate the list with the values from the reclassified raster 
for (i in 1:length(hex_grid_list)){
  
  # reclassify raster based on previously established rules
  reclass_rast <- raster::reclassify(x = lc_list[[i]], #raster to reclassify
                                     rcl = reclass.rules) #rules for how to reclassify it
  
  # let's turn the raster into categorical raster
  reclass_rast <- as.factor(reclass_rast)
  
  # let's add attributes 
  attrib <- levels(reclass_rast)[[1]]
  attrib$Class <- c('no cultivated crops/pasture/hay', 'cultivated crops/pasture/hay')
  # attrib$Color <- c('#B0B0B0', '#CA9146')
  levels(reclass_rast) <- attrib
  
  # let's estimate the area and proportion of class 0 and class 1 on each buffer (polygon)
  area_test <- exact_extract(reclass_rast, 
                             hex_grid_list[[i]], 
                             coverage_area = TRUE, 
                             summarize_df = TRUE, 
                             fun = sum_cover)
  names(area_test) <- hex_grid_list[[i]]$id
  # from list to df 
  ftest <- bind_rows(area_test, .id = 'id')
  # let's put the values into the list 
  ag_list[[i]] <- ftest |> 
    tidyr::pivot_wider(names_from = value, 
                       values_from = c(total_area, proportion)) |> 
    dplyr::select(id, total_area_1, proportion_1) |> 
    rename(ag_prop = proportion_1, 
           ag_area = total_area_1) |> 
    mutate_at(vars(ag_prop, ag_area), ~replace(., is.na(.), 0))
  
}

write_rds(ag_list, './rasters/ag_list.rds')

ag_list <- map(
  .x = ag_list, 
  .f = ~ .x |> 
    mutate(id = as.integer(id))
)

# Left join the hexagon list to the impervious values list to have all the hexagons 
ag_list <- map2(
  .x = hex_grid_list, 
  .y = ag_list, 
  .f = left_join, 
  by = join_by("id")
)

# These are sf data frames

write_rds(ag_list, './rasters/ag_list.rds')

## Scale the prop of ag values by city -----------------------------------
scale_ag_values <- data.frame(city_uwin = c('boma', 'chil', 'deio', 'inin', 'ioio', 'jams', 'lrar', 'naca', 'wide'), 
                               fcity = c('Boston, MA', 'Chicago, IL', 'Des Moines, IA',"Indianapolis, IN", 
                                         'Iowa City, IA', 'Jackson, MS', "Little Rock, AK", 'Washington DC', 
                                         'Wilmington, DE'), 
                               mean_city_ag_value = c(0.013669946, 0.011295547, 0.181566339, 0.106357617, 
                                                      0.235903195, 0.072701196, 0.127484137, 0.0059777, 0.089811437), 
                               global_sd = rep(0.143422893, 9))

scale_ag_values_list <- split(scale_ag_values, scale_ag_values$city_uwin)

ag_list_scaled <- map2(
  .x = ag_list, 
  .y = scale_ag_values_list, 
  .f = left_join, by = join_by("city_uwin")
)

ag_list_scaled <- map(
  .x = ag_list_scaled, 
  .f = ~ mutate(.x, 
                scale_ag_value = (ag_prop - mean_city_ag_value)/global_sd) 
)

write_rds(ag_list_scaled, './rasters/ag_scaled.rds') 


# Habitat ------------------------------------------------------------------
# land cover raster of all the us
lc_raster <- raster("./rasters/nlcd_2021_land_cover_l48_20230630/nlcd_2021_land_cover_l48_20230630.img")
st_crs(lc_raster)$proj4string
# "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Crop raster by state and save each individual raster
lc_list <-  vector('list', length = length(my_states_list))
names(lc_list) <- names(my_states_list)

for (i in 1:length(lc_list)){
  lc_list[[i]] <- terra::crop(lc_raster, my_states_list[[i]], mask = T)
  writeRaster(lc_list[[i]], 
              filename = paste0("./rasters/landcover_states/", names(lc_list)[[i]], "_landcover_crop.tif"), 
              format="GTiff", 
              overwrite=TRUE)
}

# Read rasters by state and load them in a list
path_rst <- list.files('./rasters/landcover_states/', full.names = TRUE)
path_rst_names <- list.files('./rasters/landcover_states/', full.names = FALSE)
path_rst_names <-  stringr::str_extract(path_rst_names,  "^[^_]+")
lc_list <- lapply(path_rst, raster)
names(lc_list) <- path_rst_names


# Reorganize raster list so it matches the city_uwin order 
new_names <- c("Massachusetts", "Illinois",  "Iowa", "Indiana", "Iowa", "Mississippi", "Arkansas", "dmv", "Delaware")
lc_list <- lc_list[c(new_names)]

#develop set of rules for reclassifying the raster to indicate which classes should be selected
reclass.rules <- as.data.frame(matrix(c(11, 0, #open water
                                        21, 1, #developed, open space
                                        22, 1, #developed, low intensity
                                        23, 0, #developed, medium intensity
                                        24, 0, #developed, high intensity
                                        31, 0, #barren land
                                        41, 1, #deciduous forest
                                        42, 1, #evergreen forest
                                        43, 1, #mixed forest
                                        52, 1, #shrub/scrub
                                        71, 1, #grassland/herbaceous
                                        81, 0, #pasture/hay
                                        82, 0, #cultivated crops
                                        90, 1, #woody wetlands
                                        95, 0), #emergent herbaceous wetlands
                                      ncol = 2, #column 1 is original cell value, column 2 is new cell value
                                      byrow = TRUE)) #tells R to fill the matrix by column, not by row

# retitle matrix - more to keep things straight than anything else
names(reclass.rules) <- c("ID", "reclass")

## Create a list into which we will add all the covariate values  
habitat_list <- vector('list', length = length(hex_grid_list))
names(habitat_list) <- names(hex_grid_list)  

# Function to summarise area and proportion of each class in raster for each hexagon
sum_cover <- function(x){
  list(x %>%
         group_by(value) %>%
         summarize(total_area = sum(coverage_area)) %>%
         mutate(proportion = total_area/sum(total_area)))
}

# Let's populate the list with the values from the reclassified raster 
for (i in 1:length(hex_grid_list)){
  
  # reclassify raster based on previously established rules
  reclass_rast <- raster::reclassify(x = lc_list[[i]], #raster to reclassify
                                     rcl = reclass.rules) #rules for how to reclassify it
  
  # let's turn the raster into categorical raster
  reclass_rast <- as.factor(reclass_rast)
  
  # let's add attributes 
  attrib <- levels(reclass_rast)[[1]]
  attrib$Class <- c('no cultivated crops/pasture/hay', 'cultivated crops/pasture/hay')
  # attrib$Color <- c('#B0B0B0', '#CA9146')
  levels(reclass_rast) <- attrib
  
  # let's estimate the area and proportion of class 0 and class 1 on each buffer (polygon)
  area_test <- exact_extract(reclass_rast, 
                             hex_grid_list[[i]], 
                             coverage_area = TRUE, 
                             summarize_df = TRUE, 
                             fun = sum_cover)
  names(area_test) <- hex_grid_list[[i]]$id
  # from list to df 
  ftest <- bind_rows(area_test, .id = 'id')
  # let's put the values into the list 
  habitat_list[[i]] <- ftest |> 
    tidyr::pivot_wider(names_from = value, 
                       values_from = c(total_area, proportion)) |> 
    dplyr::select(id, total_area_1, proportion_1) |> 
    rename(habitat_prop = proportion_1, 
           habitat_area = total_area_1) |> 
    mutate_at(vars(habitat_prop, habitat_area), ~replace(., is.na(.), 0))
  
}

write_rds(habitat_list, './rasters/habitat_list.rds')

habitat_list <- map(
  .x = habitat_list, 
  .f = ~ .x |> 
    mutate(id = as.integer(id))
)

# Left join the hexagon list to the impervious values list to have all the hexagons 
habitat_list <- map2(
  .x = hex_grid_list, 
  .y = habitat_list, 
  .f = left_join, 
  by = join_by("id")
)
# These are sf data frames

write_rds(habitat_list, './rasters/habitat_list.rds')

## Scale the prop of habitat values by city -----------------------------------
scale_habitat_values <- data.frame(city_uwin = c('boma', 'chil', 'deio', 'inin', 'ioio', 'jams', 'lrar', 'naca', 'wide'), 
                              fcity = c('Boston, MA', 'Chicago, IL', 'Des Moines, IA',"Indianapolis, IN", 
                                        'Iowa City, IA', 'Jackson, MS', "Little Rock, AK", 'Washington DC', 
                                        'Wilmington, DE'), 
                              mean_city_habitat_value = c(0.533400656, 0.374708807, 0.493558715, 0.532362766,
                                                          0.479626581, 0.637997999, 0.450166175, 0.649873011, 0.593620314), 
                              global_sd = rep(0.245040978, 9))

scale_habitat_values_list <- split(scale_habitat_values, scale_habitat_values$city_uwin)

habitat_list_scaled <- map2(
  .x = habitat_list, 
  .y = scale_habitat_values_list, 
  .f = left_join, by = join_by("city_uwin")
)

habitat_list_scaled <- map(
  .x = habitat_list_scaled, 
  .f = ~ mutate(.x, 
                scale_habitat_value = (habitat_prop - mean_city_habitat_value)/global_sd) 
)

write_rds(habitat_list_scaled, './rasters/habitat_scaled.rds') 

# END --------------------------------------------------------------------------