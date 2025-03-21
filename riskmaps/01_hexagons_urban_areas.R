library(sf)
library(dplyr)
library(tmap)
library(purrr)
library(exactextractr)
library(ggplot2)
library(readr)

# URban areas in the US --------------------------------------------------------
# Data taken from the US Census Bureau https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

urban <- st_read('./shapefiles/cb_2018_us_ua10_500k/cb_2018_us_ua10_500k.shp')
st_crs(urban)$proj4string # "+proj=longlat +datum=NAD83 +no_defs"

my_urban <- c('Boston, MA--NH--RI', 'Chicago, IL--IN', 'Des Moines, IA', 'Indianapolis, IN',
              'Iowa City, IA', 'Jackson, MS', 'Little Rock, AR',   
              'Washington, DC--VA--MD', 'Philadelphia, PA--NJ--DE--MD')

urban <- urban |> 
  filter(NAME10 %in% my_urban)

# Reproject to m to be able to calculate hexagons
rp <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
urban_rp <- st_transform(urban, crs = rp)
st_crs(urban_rp)$proj4string

# make a list by city
urban_list <- split(urban_rp, urban_rp$NAME10)

# Hexagons --------------------------------------------------------------------
## States are in m, check projection 
# Create a hexagon grid for each state
hex_grid_list <- purrr::map(
  .x = urban_list, 
  .f = ~st_make_grid(
    .x, 
    cellsize = units::as_units(1, "km^2"), 
    what = "polygons", 
    square = FALSE) |> 
    sf::st_intersection(sf::st_buffer(.x, 0)) |> 
    sf::st_as_sf() |> 
    dplyr::mutate(id = row_number()) |> 
    sf::st_make_valid()
)

# Reproject to NAD83 again
# hex_grid_list <- purrr::map(
#   .x = hex_grid_list, 
#   .f = ~st_transform(.x, crs = "+proj=longlat +datum=NAD83 +no_defs")
# )

# Convert list into data frame and export it as .rds object 
hex_grid_df <-  hex_grid_list %>%
  list_rbind(names_to = "urban_name") |> 
  sf::st_as_sf(sf_column_name = 'x', 
               crs = rp) |> 
  # delete rows that are POINT
  mutate(type = st_geometry_type(x)) |> 
  filter(type != 'POINT')

class(hex_grid_df)
st_crs(hex_grid_df)$proj4string

# Export the sf data.frame as an rds file 
readr::write_rds(hex_grid_df, 
                 './shapefiles/hex_grid_urban.rds')

# Export the shapefile 
st_write(hex_grid_df, 
         './shapefiles/hex_grid_urban.shp', 
         append = FALSE)

# this has this projection 
# "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

## END -------------------------------------------------------------------------

 
