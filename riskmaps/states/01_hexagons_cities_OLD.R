library(sf)
library(dplyr)
library(tmap)
library(purrr)
library(exactextractr)
library(ggplot2)
library(readr)


us_states2 <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) 
us_states2$ID <- stringr::str_to_title(us_states2$ID)
my_states <- us_states2 |> 
  filter(ID %in% c("Illinois", "Indiana", "Iowa", "Arkansas", "Maryland", "District Of Columbia", 
                   "Delaware", "Virginia", "Mississippi", "Massachusetts")) 
my_states_ro <- my_states |> 
  mutate(count = c(1, 1, 1, 1, 1, 2, 1, 1, 1, 1)) |> 
  tidyr::uncount(count) 

# Change names of Iowa
my_states_ro[6, ]$ID <- 'Iowa01'
my_states_ro[7, ]$ID <- 'Iowa02'

my_states_ro <- my_states_ro |> 
  mutate(city = case_match(ID, 
                           "Illinois" ~ 'chil', 
                           "Indiana" ~ 'inin', 
                           "Arkansas" ~ 'lrar', 
                           "Maryland" ~ 'naca', 
                           "District Of Columbia" ~ 'naca', 
                           "Delaware" ~ 'wide', 
                           "Virginia" ~ 'naca', 
                           "Mississippi" ~ 'jams', 
                           "Massachusetts" ~ 'boma',
                           "Iowa01" ~ 'deio', 
                           "Iowa02" ~ 'ioio'))

# raster projection 
# rp <- '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'
rp <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

my_states_rp <- st_transform(my_states_ro, crs = rp)

st_write(my_states_rp, "./shapefiles/my_states_rp.shp")

# divide states into a list to work with rasters for each city 
my_states_list <- split(my_states_rp, my_states_rp$city)

# Uwin points 
points_uwin <- st_read('./shapefiles/points_unique_sites.shp') 
points_uwin_rp <- st_transform(points_uwin, crs = rp)

# Divide points into a list for each city 
points_list <- split(points_uwin_rp, f = points_uwin_rp$city)

# class(my_states_rp)
# st_crs(my_states_rp)$proj4string 

# Hexagons ---------------------------------------------
## States are in m, check projection 
# Create a hexagon grid for each state
hex_grid_list <- purrr::map(
  .x = my_states_list, 
  .f = ~st_make_grid(
    .x, 
    cellsize = units::as_units(5, "km^2"), 
    what = "polygons", 
    square = FALSE) |> 
    sf::st_intersection(sf::st_buffer(.x, 0)) |> 
    sf::st_as_sf() |> 
    dplyr::mutate(id = row_number()) |> 
    sf::st_make_valid()
)

plot(hex_grid_list[[5]])

# Convert list into data frame and export it as .rds object 
hex_grid_df <-  hex_grid_list %>%
  imap_dfr(~ mutate(.x, city_uwin = .y)) |> 
  # one row in jams is a POINT so let's delete it
  mutate(type = st_geometry_type(x)) |> 
  filter(type != 'POINT')
  
  
readr::write_rds(hex_grid_df, 
                 './shapefiles/hex_grid_states.rds')

## END -------------------------------------------------------------------------

 
