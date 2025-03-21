## Script to extract population density variables into a hexagonal grid for each state 
## US census data accessed through tidycensus 

library(tidycensus)
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

# US States Shapefiles ---------------------------------------------------------
my_states <- states(cb = TRUE) |> 
  filter(NAME %in% c("Illinois", "Indiana", "Iowa", "Arkansas", "Maryland", "District of Columbia", 
                     "Delaware", "Virginia", "Mississippi", "Massachusetts")) 
st_crs(my_states)$proj4string # "+proj=longlat +datum=NAD83 +no_defs"

my_states_list <- split(my_states, my_states$NAME)

# Combine VA, MD, and DC 
dc <- my_states_list[["District of Columbia"]]
va <- my_states_list[["Virginia"]]
md <- my_states_list[["Maryland"]]

my_states_list[["dmv"]] <- rbind(dc, va, md)
my_states_list <- purrr::list_modify(my_states_list, "Virginia" = NULL)
my_states_list[["Virginia"]] <- NULL 
my_states_list[["Maryland"]] <- NULL
my_states_list[["District of Columbia"]] <- NULL

# Hex grid States ---------------------------------------------------------------------
hex_grid <- readr::read_rds('./shapefiles/hex_grid_states.rds') 
st_crs(hex_grid)$proj4string
# Reproject to NAD83, same as states and tidycensus
hex_grid_nad83 <- st_transform(hex_grid, crs = "+proj=longlat +datum=NAD83 +no_defs +type=crs")

# Split the df into a list by city 
hex_grid_list <- split(hex_grid_nad83, hex_grid_nad83$city_uwin)
list_names <- names(hex_grid_list)

# Create a list in which to store the covariate values for each city 
df_list <- vector('list', length = length(hex_grid_list))
names(df_list) <- names(hex_grid_list)  

# tmap_mode("view")
# tm_shape(my_states)+
#   tm_fill()+
#   tm_shape(hex_grid_nad83)+
#   tm_polygons()



# Extract population values from US Census -------------------------------------
## Avoid scientific notation 
options(scipen = 999)

# Connect to the Census API ----------------------------------------------------
# http://api.census.gov/data/key_signup.html 

# API Key 
census_api_key("656321a3edf01184e1eef7223c905427395d198f")

# List of cities 
states <- c("MA", "NH", "RI", "IL", "IA", "IN", "MS", 
            "AR", "DC", "MD", "VA", "DE", "PA", "NJ")

# List of variables to extract from the census API
TotalPop20      = "P2_001N" # Total Population

## ALAND20 is the land area in m2 
## to convert square meters to square kilometers, divide by 1,000,000; 
## to convert square kilometers to square miles, divide by 2.58999; 
## to convert square meters to square miles, divide by 2,589,988
km_2 <- 1000000

## Population density ----------------------------------------------------------
us_county_density <- get_decennial(
  geography = "block",
  state = states,
  variables = TotalPop20, # see list above
  year = 2020,
  geometry = TRUE,
  keep_geo_vars = TRUE
) 

## Estimate housing density per square km
us_county_density <- us_county_density %>% 
  mutate(pop_density = value  / (ALAND20 / km_2)) %>% 
  mutate(house_density = HOUSING20 / (ALAND20) / km_2) |> 
  mutate(state_name = case_when(STATEFP20 == '05' ~ 'AR', 
                                STATEFP20 == '10' ~ 'DE', 
                                STATEFP20 == '11' ~ 'DC', 
                                STATEFP20 == '17' ~ 'IL',
                                STATEFP20 == '18' ~ 'IN',
                                STATEFP20 == '19' ~ 'IA', 
                                STATEFP20 == '25' ~ 'MA', 
                                STATEFP20 == '24' ~ 'MD',
                                STATEFP20 == '28' ~ 'MS', 
                                STATEFP20 == '51' ~ 'VA', 
                                STATEFP20 == '33' ~ 'NH',
                                STATEFP20 == '44' ~ 'RI',
                                STATEFP20 == '42' ~ 'PA', 
                                STATEFP20 == '34' ~ 'NJ', 
                                TRUE ~ STATEFP20)) %>% 
  relocate(pop_density, .after = value) %>% 
  relocate(state_name, .after = STATEFP20)

## save RDS file with pop density and house density in 2020 across 11 states ---
saveRDS(us_county_density, './covariates/us_county_urban_density.rds')

# us_county_density <- readr::read_rds('./covariates/us_county_urban_density.rds')

class(us_county_density)
st_crs(us_county_density)$proj4string # NAD83

# Urban shapefiles -------------------------------------------------------------
urban <- st_read('./shapefiles/cb_2018_us_ua10_500k/cb_2018_us_ua10_500k.shp')
st_crs(urban)$proj4string # "+proj=longlat +datum=NAD83 +no_defs"

# Reproject to rasters projection 
rp <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
urban_rp <- st_transform(urban, crs = rp)
st_crs(urban_rp)$proj4string

# Vector with my study sites 
my_urban <- c('Boston, MA--NH--RI', 'Chicago, IL--IN', 'Des Moines, IA', 'Indianapolis, IN',
              'Iowa City, IA', 'Jackson, MS', 'Little Rock, AR',   
              'Washington, DC--VA--MD', 'Philadelphia, PA--NJ--DE--MD')

urban_rp <- urban_rp |> 
  filter(NAME10 %in% my_urban)

## LIST PART -------------------------------------------------------------------
# Make it a list 
urban_list <- split(urban_rp, urban_rp$NAME10)

# Reorganize names in list 
new_names <- c('Boston, MA--NH--RI', 'Chicago, IL--IN', 'Des Moines, IA', 'Indianapolis, IN',  
               'Iowa City, IA', 'Jackson, MS', 'Little Rock, AR',  
               'Washington, DC--VA--MD', 'Philadelphia, PA--NJ--DE--MD')
urban_list <- urban_list[c(new_names)]

# Rename list to match uwin names 
names(urban_list) <- c('boma', 'chil', 'deio', 'inin', 'ioio', 'jams', 
                       'lrar', 'naca', 'wide')

# Hex grid urban ---------------------------------------------------------------
hex_grid <- sf::st_read('./shapefiles/hex_grid_urban.shp') 
st_crs(hex_grid)$proj4string

# hex_grid_rp <- st_transform(hex_grid, crs = rp)
# st_crs(hex_grid_rp)$proj4string

## LIST PART -------------------------------------------------------------------
# Split the df into a list by city 
hex_grid_list <- split(hex_grid_rp, hex_grid_rp$urban_name)

# Rename list to match uwin names 
names(hex_grid_list) <- c('boma', 'chil', 'deio', 'inin', 'ioio', 'jams', 
                          'lrar', 'naca', 'wide')

# US Census --------------------------------------------------------------------
## open RDS file with pop density and house density in 2020 across 11 states ---
# pop_density <- read_rds('./covariates/us_county_urban_density.rds')
# st_crs(pop_density)$proj4string # "+proj=longlat +datum=NAD83 +no_defs"
# class(pop_density) #sf data.frame

# reproject to match hex grid projection 
pop_density_rp <- st_transform(us_county_density, crs = rp)
st_crs(pop_density_rp)$proj4string

# subset the density values that are inside our hexagons per urban area 
pop_hex_subset <- pop_density_rp[hex_grid, ]

write_rds(pop_hex_subset, './shapefiles/pop_hex_subset.rds')
pop_hex_subset <- read_rds('./shapefiles/pop_hex_subset.rds')
remove(pop_hex_subset)
gc()

## LIST PART -------------------------------------------------------------------
# Make it a list per state of interest 
pop_hex_list <- split(pop_hex_subset, pop_hex_subset$state_name)

# combine states to make DMV
dc <- pop_hex_list[["DC"]]
va <- pop_hex_list[["VA"]]
md <- pop_hex_list[["MD"]]
pop_hex_list[["dmv"]] <- rbind(dc, va, md)

# Combine states to make MA--NH--RI
ma <- pop_hex_list[["MA"]]
nh <- pop_hex_list[["NH"]]
ri <- pop_hex_list[["RI"]]
pop_hex_list[["ma-nh-ri"]] <- rbind(ma, nh, ri)

# Combine states to make IL--IN
il <- pop_hex_list[["IL"]]
indiana <- pop_hex_list[["IN"]]
pop_hex_list[["il-in"]] <- rbind(il, indiana)

# combine states to make PA--NJ--DE--MD
pa <- pop_hex_list[["PA"]]
nj <- pop_hex_list[["NJ"]]
de <- pop_hex_list[["DE"]]
pop_hex_list[["pa-nj-de-md"]] <- rbind(pa, nj, de, md)

pop_hex_list[["DC"]] <- NULL
pop_hex_list[["VA"]] <- NULL
pop_hex_list[["MD"]] <- NULL
pop_hex_list[["MA"]] <- NULL
pop_hex_list[["NH"]] <- NULL
pop_hex_list[["RI"]] <- NULL
pop_hex_list[["IL"]] <- NULL
pop_hex_list[["PA"]] <- NULL
pop_hex_list[["NJ"]] <- NULL
pop_hex_list[["DE"]] <- NULL

# Reorganize names in list 
new_names <- c("ma-nh-ri", "il-in", "IA", "IN", "IA", "MS", "AR", "dmv", "pa-nj-de-md")
pop_hex_list <- pop_hex_list[c(new_names)]
# Rename list to match uwin names 
names(pop_hex_list) <- c('boma', 'chil', 'deio', 'inin', 'ioio', 'jams', 
                          'lrar', 'naca', 'wide')

# Crosswalk analysis by urban area ---------------------------------------------

# remove some files to free up memory
remove(us_county_density, urban_rp, urban)


# Make geometries valid 
hex_grid <- st_make_valid(hex_grid)
pop_hex_subset <- st_make_valid(pop_hex_subset)

gc()

# my_urban <- c('Boston, MA--NH--RI', 'Chicago, IL--IN', 'Des Moines, IA', 'Indianapolis, IN',
#               'Iowa City, IA', 'Jackson, MS', 'Little Rock, AR',   
#               'Washington, DC--VA--MD', 'Philadelphia, PA--NJ--DE--MD')
# Extract only one city at a time 

city_pop <- pop_hex_subset |> 
  filter(state_name %in% c("PA", "NJ", "DE", "MD"))

unique(hex_grid$urban_name)
city_hex <- hex_grid |> 
  filter(urban_name %in% c("Philadelphia, PA--NJ--DE--MD" )) 

city_pop <- st_make_valid(city_pop)
city_hex <- st_make_valid(city_hex)

start <- Sys.time()

library(foreach)
library(doParallel)

# Set number of cores
n.cores <- parallel::detectCores()-10 # 20 cores, but just use 12
# Create the cluster 
my.cluster <- parallel::makeCluster(
  n.cores,
  type = 'PSOCK'
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

# Create an empty matrix to put all the pop density values in 
final_mx <- matrix(data = NA, nrow = nrow(city_hex), ncol = 1)

# Crosswalk analysis by city in parallel 
final_mx <- foreach(i=1:nrow(city_hex), 
                    .packages = c('raster', 'dplyr', 'tidyr', 'sf', 'units')) %dopar% {
          
                      temp <- city_pop[city_hex[i,],]
                      
                      intersect_prop <- st_intersection(temp, city_hex[i,]) %>%  
                        mutate(intersect_area = st_area(.),                
                               block_area = st_area(temp),
                               intersect_prop = intersect_area/block_area) %>%
                        # drop the geometry
                        st_drop_geometry(.)   %>%
                        # keep only the proportion of overlap
                        pull(intersect_prop)
                      
                      # create new variables that multiply the demographic data by the prop coverage
                      hb_pop <- temp |> 
                        mutate(pop = POP20*intersect_prop)  |> # proportion of people living in this census block assuming that people are homogeneously living in this block
                        pull(pop) |> 
                        units::drop_units()
                      
                      # get the sum of all the people living under this buffer
                      # pop <- apply(pop_values, 2, sum, na.rm = TRUE)
                      pop <- sum(hb_pop, na.rm = TRUE)
                      
                      # divide the mean by the holc polygon area to get population density for 
                      # each polygon,  unit is km2
                      popdens <- pop / (as.numeric(st_area(city_hex[i,])) / 1e6)
                      
                      final_mx[i, ] <- popdens
                    }

# Stop the cluster at the end
parallel::stopCluster(cl = my.cluster)

stop <- Sys.time()

stop - start 

final_mx <- as.data.frame(do.call(rbind, final_mx)) |> 
  tibble::rownames_to_column(var = "id") |> 
  rename("pop_dens" = "V1")

readr::write_rds(final_mx, './covariates/pop_urban/pop_matrix_urban_wide_ns.rds')


# Bring all cities together into one single df 
path_rst <- list.files('./covariates/pop_urban/', full.names = TRUE)
path_rst_names <- list.files('./covariates/pop_urban/', full.names = FALSE)
path_rst_names <-  stringr::str_extract(path_rst_names, "(?<=pop_matrix_urban_).+?(?=_ns\\.rds)")
lc_list <- lapply(path_rst, readr::read_rds)
names(lc_list) <- path_rst_names

# Transform list to a data frame with the list names
pop_df <- map_dfr(lc_list, ~tibble(value = .x), .id = "city_uwin")
pop_df <- do.call(data.frame, pop_df)

# Fix the names of deio-lrar 
pop_df <- pop_df |> 
  mutate(city_uwin_fix = rep(c('boma', 'chil', 'deio', 'inin', 'ioio', 'jams', 'lrar', 'naca', 'wide'), c(6312, 7413, 709, 2377, 177, 915, 1012, 4128, 6494)), 
         id = sequence(c(6312, 7413, 709, 2377, 177, 915, 1012, 4128, 6494))) |> 
  dplyr::select(id, city_uwin_fix, value.pop_dens) |> 
  rename(city_uwin = city_uwin_fix) |> 
  rename(pop_dens = value.pop_dens)

# Add city and hexagon number by joining with hex_grid_rp
# Add uwin_city
uwin_city <- data.frame(city_uwin = c('boma', 'chil', 'deio', 'inin', 'ioio', 'jams', 'lrar', 'naca', 'wide'), 
                        fcity = c('Boston, MA', 'Chicago, IL', 'Des Moines, IA',"Indianapolis, IN", 
                                  'Iowa City, IA', 'Jackson, MS', "Little Rock, AK", 'Washington DC', 
                                  'Wilmington, DE'), 
                        urban_name = c('Boston, MA--NH--RI', 'Chicago, IL--IN', 'Des Moines, IA', 'Indianapolis, IN',  
                                       'Iowa City, IA', 'Jackson, MS', 'Little Rock, AR',  
                                       'Washington, DC--VA--MD', 'Philadelphia, PA--NJ--DE--MD'))

pop_df <- left_join(pop_df, uwin_city)

# Left join by urban_name 
pop_df <- left_join(pop_df, hex_grid)
class(pop_df)

# Scale population density variable by city
scaling_values <- data.frame(city_uwin = c('boma', 'chil', 'deio', 'inin', 'ioio', 'jams', 'lrar', 'naca', 'wide'), 
                             fcity = c('Boston, MA', 'Chicago, IL', 'Des Moines, IA',"Indianapolis, IN", 
                                       'Iowa City, IA', 'Jackson, MS', "Little Rock, AK", 'Washington DC', 
                                       'Wilmington, DE'), 
                             mean_city_pop = c(7.105757611, 6.772505076, 4.976265738, 6.302098395,
                                                     5.336136434, 6.119535182, 4.563478307, 7.548228975,
                                                     6.204623927), 
                             global_sd = rep(1.82990811202879, 9))
# join the scaling_values data frame with the buf_popdens data frame 
scaled_bufpopdens <- left_join(pop_df, scaling_values, by = join_by("city_uwin", "fcity"))

scaled_bufpopdens <- scaled_bufpopdens |> 
  mutate(sc_log_pop = ((log(pop_dens + 1)) - mean_city_pop)/global_sd) 

class(scaled_bufpopdens)

write_rds(scaled_bufpopdens, './rasters/final_scaled_urban_covariates/popdens_urban_scaled.rds')

# Shapefile 
pop_st <- st_as_sf(scaled_bufpopdens)
class(pop_st)
st_crs(pop_st)$proj4string
st_write(pop_st, './shapefiles/urban_covariates_scaled/popdens_urban_scaled.shp')


