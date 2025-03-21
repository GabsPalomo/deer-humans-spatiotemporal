# Packages ---------------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(cowplot)
library(MCMCvis)
library(kableExtra)
library(ggridges)
library(sf)
library(stringr)
library(tmap)

# don't use scientific notation 
options(scipen=999)

## Human data ------------------------------------------------------------------
## days cameras were active across all sites and seasons
# [-1] is to remove the first column which has site names 
human_jmat <- read_csv('./spatial_analysis/model_matrices/human_jmat.csv')
## deer detections across all sites and seasons
# [-1] is to remove the first column which has site names 
human_detections <- read_csv('./spatial_analysis/model_matrices/human_det.csv')

## Models outputs
mcmc.output <- readr::read_rds('./spatial_analysis/rds_model/human_mcmc_output_01_2024-10-18.rds')

# Hex values covariates 
cov_mat_full <- read_csv('./covariates/final_urban_scaled_covariates.csv') 
cov_mat_full <- cov_mat_full |> 
  mutate(urban_name = case_match(city_uwin, 
                                 "boma" ~ "Boston, MA--NH--RI",
                                 "chil" ~"Chicago, IL--IN", 
                                 "deio" ~"Des Moines, IA", 
                                 "inin" ~ "Indianapolis, IN", 
                                 "ioio" ~"Iowa City, IA", 
                                 "jams" ~"Jackson, MS", 
                                 "lrar" ~"Little Rock, AR", 
                                 "naca" ~"Washington, DC--VA--MD",
                                 "wide" ~"Philadelphia, PA--NJ--DE--MD"))

cov_mat <- cov_mat_full |> 
  # filter(city_uwin == "deio") |>
  # Weirdly I had two NA values in rows 13725 and 29537 so I am making them 0 to avoid issues in for loop
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |> 
  select(!c(id, city_uwin, urban_name)) |> 
  mutate(temp = 1)

# put scaled x in a matrix 
x_mat <- as.matrix(cov_mat)

# Random sample of Posterior ---------------------------------------------------
# Put human model betas in a matrix and take a random sample of 10K
beta_mat <- do.call(rbind, mcmc.output)
set.seed(787)
sim_subset <- beta_mat[sample(1:nrow(beta_mat), 10000),]

# extract only the covariate betas and mu_city
colnames(sim_subset)
cov_beta_mat <- as.data.frame(sim_subset[, c("mu_city", 
                               "beta_ag", "beta_habitat", "beta_impervious", "beta_pop", "beta_temp")])

beta_list <- as.list(cov_beta_mat)

# For the offset, estimate the mean of the entire _jmat matrix 
# Except the first column which is siteID
human_jmat_mean <- data.frame(human_jmat[-1]) |> 
  summarise_all(list(~mean(., na.rm = TRUE))) |> 
  rowMeans()

human_jmat_mean

ncell <- nrow(x_mat)
pred_results <- matrix(NA, nrow = nrow(x_mat), ncol=nrow(cov_beta_mat))

# Derived quantities: predictions for the entire grid
for (s in 1:ncell){
  pred_results[s,] <- exp(beta_list$mu_city + log(human_jmat_mean) + 
    beta_list$beta_ag * cov_mat$scale_ag[s] +
    beta_list$beta_habitat * cov_mat$scale_habitat[s] +
    beta_list$beta_impervious * cov_mat$scale_impervious[s] +
    beta_list$beta_pop * cov_mat$sc_log_pop[s] +
    beta_list$beta_temp * cov_mat$temp[s])
  }

post.mean <- apply(pred_results, 1, mean) # Posterior mean
post.sd <- apply(pred_results, 1, sd)     # Posterior standard deviation

results.df <- data.frame(post.mean) 
results.df <- cbind(results.df, id = cov_mat_full$id, city_uwin = cov_mat_full$city_uwin, urban_name = cov_mat_full$urban_name)

# covariates with coordinates 
hex_grid_urban <- sf::st_read('./shapefiles/hex_grid_urban.shp') 

results_map <- left_join(results.df, hex_grid_urban) |> 
  select(id, city_uwin, urban_name, post.mean, type, geometry)

results_map_st <- sf::st_as_sf(results_map)
class(results_map_st)

st_write(results_map_st, './shapefiles/results_map_humans_st.shp')

# remove rivers from DC 
dc_rivers <- sf::st_read('./shapefiles/Waterbodies.shp')
rp <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
dc_rivers <- st_transform(dc_rivers, crs = rp)
st_write(dc_rivers, './shapefiles/dc_rivers_rp.shp')

va_rivers <- st_read('./shapefiles/Rivers.shp')
va_rivers <- st_transform(va_rivers, crs = rp)
va_rivers <- st_make_valid(va_rivers)
st_write(va_rivers, './shapefiles/va_rivers_rp.shp')

results_map_clip <- st_read('./shapefiles/clipped_human_results.shp')
results_map_clip <- results_map_clip |> 
  mutate(city = case_match(city_uwin, 
                                 "boma" ~ "Boston, MA",
                                 "chil" ~"Chicago, IL", 
                                 "deio" ~"Des Moines, IA", 
                                 "inin" ~ "Indianapolis, IN", 
                                 "ioio" ~"Iowa City, IA", 
                                 "jams" ~"Jackson, MS", 
                                 "lrar" ~"Little Rock, AR", 
                                 "naca" ~"Washington, DC",
                                 "wide" ~"Wilmington, DE"))
# Create final facetted map
tmap_mode("plot")
human_map <- tm_shape(results_map_clip)+
  tm_fill(col = "post_mean", 
          breaks = c(0, 100, 200, 300, 400, 600, 800, 1000, 
                     2000, 5000, 10600), 
          palette = "viridis") +
  tm_facets(by = "city" , free.coords = TRUE) +
  tm_scale_bar(position = c("left", "bottom"), 
               text.size = 5)+
  tm_layout(panel.label.size = 1, 
            panel.label.fontface = "bold",
            panel.label.bg.color = NA, 
            legend.outside.position = "right", 
            frame = FALSE, # remove boxes around maps
            frame.lwd = NA, 
            main.title = "Cell size: 1-km2", 
            main.title.position = "right", 
            main.title.size = 1)

tmap_save(human_map, 
          "./riskmaps/human_riskmap.pdf", 
          width = 8.5, 
          height = 11, 
          units = "in",
          dpi = 300)




# Deer data ------------------------------------------------------------------
## deer detections across all sites and seasons
# [-1] is to remove the first column which has site names
deer_jmat <- read_csv('./spatial_analysis/model_matrices/deer_jmat.csv')
deer_detections <- read_csv('./spatial_analysis/model_matrices/deer_detections.csv')

# Model outputs
mcmc.output.deer <- readr::read_rds('./spatial_analysis/rds_model/deer_mcmc_output_01_2024-07-17.rds')

# Random sample of Posterior ---------------------------------------------------
# Put human model betas in a matrix and take a random sample of 10K
beta_mat <- do.call(rbind, mcmc.output.deer)
set.seed(787)
sim_subset <- beta_mat[sample(1:nrow(beta_mat), 10000),]

# extract only the covariate betas and mu_city
colnames(sim_subset)
cov_beta_mat <- as.data.frame(sim_subset[, c("mu_city", 
                                             "beta_ag", "beta_habitat", "beta_impervious", "beta_pop", "beta_temp")])

beta_list_deer <- as.list(cov_beta_mat)

# For the offset, estimate the mean of the entire _jmat matrix 
# Except the first column which is siteID
deer_jmat_mean <- data.frame(deer_jmat[-1]) |> 
  summarise_all(list(~mean(., na.rm = TRUE))) |> 
  rowMeans()

deer_jmat_mean

ncell <- nrow(x_mat)
pred_results_deer <- matrix(NA, nrow = nrow(x_mat), ncol=nrow(cov_beta_mat))

# Derived quantities: predictions for the entire grid
for (s in 1:ncell){
  pred_results_deer[s,] <- exp(beta_list_deer$mu_city + log(deer_jmat_mean) + 
    beta_list_deer$beta_ag * cov_mat$scale_ag[s] +
    beta_list_deer$beta_habitat * cov_mat$scale_habitat[s] +
    beta_list_deer$beta_impervious * cov_mat$scale_impervious[s] +
    beta_list_deer$beta_pop * cov_mat$sc_log_pop[s] +
    beta_list_deer$beta_temp * cov_mat$temp[s])
}

post.mean.deer <- apply(pred_results_deer, 1, mean) # Posterior mean
post.sd.deer <- apply(pred_results_deer, 1, sd)     # Posterior standard deviation

results.df.deer <- data.frame(post.mean.deer) 
results.df.deer <- cbind(results.df.deer, id = cov_mat_full$id, city_uwin = cov_mat_full$city_uwin, urban_name = cov_mat_full$urban_name)

# covariates with coordinates 
hex_grid_urban <- sf::st_read('./shapefiles/hex_grid_urban.shp') 

results_map_deer <- left_join(results.df.deer, hex_grid_urban) |> 
  select(id, city_uwin, urban_name, post.mean.deer, type, geometry)

results_map_deer_st <- sf::st_as_sf(results_map_deer)

st_write(results_map_deer_st, './shapefiles/results_map_deer_st.shp')

# Export map that has been clipped in QGIS because it's faster than 
# Doing it in R with sf::st_difference()
results_deer_map_clip <- st_read('./shapefiles/clipped_deer_results.shp')
results_deer_map_clip <- results_deer_map_clip |> 
  mutate(city = case_match(city_wn, 
                           "boma" ~ "Boston, MA",
                           "chil" ~"Chicago, IL", 
                           "deio" ~"Des Moines, IA", 
                           "inin" ~ "Indianapolis, IN", 
                           "ioio" ~"Iowa City, IA", 
                           "jams" ~"Jackson, MS", 
                           "lrar" ~"Little Rock, AR", 
                           "naca" ~"Washington, DC",
                           "wide" ~"Wilmington, DE")) |> 
  rename(city_uwin = city_wn, 
         urban_name = urbn_nm, 
         post_mean = pst_mn_)

# Create final facetted map
tmap_mode("plot")
deer_map <- tm_shape(results_deer_map_clip)+
  tm_fill(col = "post_mean", 
          palette = "viridis") +
  tm_facets(by = "city" , 
            free.coords = TRUE) +
  tm_scale_bar(position = c("left", "bottom"), 
               text.size = 2)+
  tm_layout(panel.label.size = 1, 
            panel.label.fontface = "bold",
            panel.label.bg.color = NA, 
            legend.outside.position = "right", 
            frame = FALSE, # remove boxes around maps
            frame.lwd = NA, 
            main.title = "Cell size: 1-km2", 
            main.title.position = "right", 
            main.title.size = 1)

tmap_save(deer_map, 
          "./riskmaps/deer_riskmap.pdf", 
          width = 8.5, 
          height = 11, 
          units = "in",
          dpi = 300)



library(tmap)

tmap_mode("view")
tm_shape(results_map_deer_st)+
  tm_fill(col = "post.mean.deer", 
          palette = "viridis")+
  tm_facets(by = "urban_name")
  
