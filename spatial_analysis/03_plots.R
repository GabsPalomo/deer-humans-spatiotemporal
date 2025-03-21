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
library(stringr)

# don't use scientific notation 
options(scipen=999)

# Data ------------------------------------------------------------------------
## Human data -----------------------------------------------------------------
## days cameras were active across all sites and seasons
# [-1] is to remove the first column which has site names 
human_jmat <- read_csv('./spatial_analysis/model_matrices/human_jmat.csv')
 
human_detections <- read_csv('./spatial_analysis/model_matrices/human_det.csv')

## Deer data -------------------------------------------------------------------
## deer detections across all sites and seasons
# [-1] is to remove the first column which has site names
deer_jmat <- read_csv('./spatial_analysis/model_matrices/deer_jmat.csv')
deer_detections <- read_csv('./spatial_analysis/model_matrices/deer_detections.csv')

## old locations 
# deer_jmat <- read_csv('F:/UMD/Temporal_humans_deer/data_model/deer_jmat.csv')
# deer_detections <- read_csv('F:/UMD/Temporal_humans_deer/data_model/deer_detections.csv')

## Covariates ------------------------------------------------------------------ 
cov_mat <- read_csv('./spatial_analysis/model_matrices/cov_mat.csv')
temp_mat <- readr::read_csv('./spatial_analysis/model_matrices/temp_mat_unique_ij.csv')[, -1]
ndvi_mat <- read_csv('./spatial_analysis/model_matrices/UWIN.NDVI.forGabby_2024.07.16.csv')[, 8:23]

## Models outputs----------------------------------------------------------------
mcmc.output <- readr::read_rds('./spatial_analysis/rds_model/human_mcmc_output_01_2024-10-18.rds')
mcmc.output.deer <- readr::read_rds('./spatial_analysis/rds_model/deer_mcmc_output_01_2024-07-17.rds')

## old files
# mcmc.output <- readr::read_rds("F:/UMD/SafeGraph/rds_model/mcmc_output_01_2024-07-17.rds")
# mcmc.output.deer <- readRDS("F:/UMD/Temporal_humans_deer/rds/mcmc_output_01_2024-07-17.rds")

# Heatmap of detections --------------------------------------------------------
## we need to pass to long format deer and human data so we can visualize it 
## with ggplot2
## The heatmap of both species should look the same because it shows that there 
## is detection information for each city and season for both species 
deer_detections_long <- deer_detections |> 
  mutate(id = map_chr(siteID, ~ str_sub(.x, 1, 4))) |> 
  select(!c(siteID)) |> 
  pivot_longer(cols = !id, 
               names_to = 'season', 
               values_to = 'values') |> 
  group_by(id, season) |> 
  summarise(values = sum(values, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(season = factor(season, levels =  c('JA19', 'AP19','JU19','OC19',
                                             'JA20', 'AP20','JU20','OC20',
                                             'JA21', 'AP21','JU21','OC21',
                                             'JA22', 'AP22', 'JU22','OC22'))) |> 
  mutate(species = 'deer') 
  

human_detections_long <- human_detections |> 
  mutate(id = map_chr(siteID, ~ str_sub(.x, start = -4))) |> 
  select(!c(siteID)) |> 
  pivot_longer(cols = !id, 
               names_to = 'season', 
               values_to = 'values') |> 
  group_by(id, season) |> 
  summarise(values = sum(values, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(season = factor(season, levels =  c('JA19', 'AP19','JU19','OC19',
                                             'JA20', 'AP20','JU20','OC20',
                                             'JA21', 'AP21','JU21','OC21',
                                             'JA22', 'AP22', 'JU22','OC22'))) |> 
  mutate(species = 'human') 

deer_human_dets <- rbind(deer_detections_long, human_detections_long)

ggplot(deer_human_dets, 
       aes(x = id, y = season, fill = values))+
  geom_tile() +
  facet_wrap(~species, scales = "free")+
  scale_fill_gradient(limits=c(min(deer_human_dets$values)+1, 
                               max(deer_human_dets$values)), 
                      na.value = 'white')

# Distribution plots - coefficients --------------------------------------------
mcmc.output.df <- do.call(rbind, mcmc.output) |> 
  data.frame() |> 
  pivot_longer(cols = everything(), 
               names_to = "Coefficient", 
               values_to = "Value") |> 
  mutate(species = "human")

mcmc.output.deer.df <- do.call(rbind, mcmc.output.deer) |> 
  data.frame() |> 
  pivot_longer(cols = everything(), 
               names_to = "Coefficient", 
               values_to = "Value") |> 
  mutate(species = "deer")

mcmc.output.df <- rbind(mcmc.output.df, mcmc.output.deer.df)
mcmc.output.df <- mcmc.output.df |> 
  mutate(coef.name = case_match(Coefficient, 
                                'beta_temp' ~ 'temperature', 
                                'beta_pop' ~ 'human pop density', 
                                'beta_ndvi' ~ 'NDVI', 
                                'beta_impervious' ~ 'impervious cover', 
                                'beta_habitat' ~ 'habitat', 
                                'beta_ag' ~ 'agriculture'))


mcmc.output.df |> 
  filter(Coefficient %in% c('beta_ag', 'beta_habitat', 'beta_impervious', 'beta_pop', 'beta_temp', 'beta_ndvi')) |> 
  ggplot(aes(x = Value, y = coef.name, fill = species))+
  geom_density_ridges()+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
  scale_fill_manual(values = c( "#6986a9",  "#774e4e"))+
  xlab("Coefficient") +
  theme_classic() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 20, face = 'bold', margin = margin(0.6, 0, 0, 0, unit = "cm")),
        axis.text = element_text(size = 18),
        legend.title = element_blank(), 
        legend.position = "top",
        legend.text = element_text(size = 18), 
        panel.grid.major.y = element_line(color = "gray70"),
        plot.title = element_text(size = 16, hjust = 0),
        plot.title.position = "plot",
        plot.margin = margin(0, 1.5, 1.5, 1.5, unit = "cm")) -> dist.p1

dist.p1

mcmc.output.city.df <- mcmc.output.df |> 
  filter(Coefficient %in% c("beta_city.1.", "beta_city.2.", "beta_city.3.", "beta_city.4.", 
                            "beta_city.5.", "beta_city.6.", "beta_city.7.", "beta_city.8.","beta_city.9.")) |> 
  mutate(city = case_match(Coefficient, 
                           "beta_city.1." ~ "Boston MA", 
                           "beta_city.2." ~ "Chicago IL", 
                           "beta_city.3." ~ "Des Moines IA", 
                           "beta_city.4." ~ "Indianapolis IN", 
                           "beta_city.5." ~ "Iowa City IA", 
                           "beta_city.6." ~ "Jackson MS", 
                           "beta_city.7." ~ "Little Rock AR", 
                           "beta_city.8." ~ "Washington DC",
                           "beta_city.9." ~ "Wilmington DE")) |> 
  # organized by population size 
  mutate(city = factor(city, levels = c("Chicago IL", "Indianapolis IN", 
                                        "Washington DC", "Boston MA", 
                                        "Des Moines IA", "Little Rock AR", 
                                        "Jackson MS", "Iowa City IA", "Wilmington DE")))

library(latex2exp)
eq1 <- c(TeX(r"($exp^{\beta_0}$)"))

mcmc.output.city.df |> 
ggplot(aes(x = exp(Value), y = city, fill = species))+
  geom_density_ridges()+
  scale_fill_manual(values =  c( "#6986a9",  "#774e4e"))+
  scale_color_manual(values =  c( "#6986a9",  "#774e4e"))+
  facet_wrap(~species)+
  scale_y_discrete(limits = rev)+
  ggh4x::facetted_pos_scales(y = list(COL == 2 ~ scale_y_discrete(guide = 'none')))+
  lemon::facet_rep_grid(~species, scales = "free")+
  xlab(label = "Mean relative activity") +
  theme_classic() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 20, margin = margin(0.6, 0, 0, 0, unit = "cm")),
        axis.text = element_text(size = 18),
        axis.text.y = element_text(margin = margin(0, 0.3, 0, 0, 'cm')),
        legend.title = element_blank(), 
        legend.position = "top",
        legend.text = element_text(size = 18), 
        panel.grid.major.y = element_line(color = "gray70"), 
        strip.text = element_blank(), 
        plot.title.position = "plot",
        plot.margin = margin(0, 1.5, 1.5, 1.5, unit = "cm")) -> dist.p2

dist.p2

ggsave(dist.p1, 
       filename = "./spatial_analysis/model_plots/coefficient_dist_plots.png", 
       dpi = 300,
       width = 12, 
       height = 8, 
       units = c("in"))

ggsave(dist.p2, 
       filename = "./spatial_analysis/model_plots/coefficient_dist_city_plots.png", 
       dpi = 300, 
       width = 12, 
       height = 8, 
       units = c("in"))

# Coefficient plots - Error bars----------------------------------------------------
mcmc.summary.human <- MCMCvis::MCMCsummary(mcmc.output, 
                                     round = 2, 
                                     # probabilities and median
                                     probs = c(0.025, 0.5, 0.975)) |> 
  tibble::rownames_to_column("coefficient") |> 
  mutate(species = "humans") 

mcmc.summary.deer <- MCMCvis::MCMCsummary(mcmc.output.deer, 
                                     round = 2, 
                                     # probabilities and median
                                     probs = c(0.025, 0.5, 0.975)) |> 
  tibble::rownames_to_column("coefficient") |> 
  mutate(species = "deer")

mcmc.summary <- rbind(mcmc.summary.human, mcmc.summary.deer)

mcmc.summary.covar <- mcmc.summary |> 
  filter(coefficient %in% c("beta_temp", "beta_ndvi", 
                            "beta_ag", "beta_pop", "beta_impervious", "beta_habitat")) |> 
  mutate(covar = case_match(coefficient, 
                            "beta_temp" ~ "temperature", 
                            "beta_ndvi" ~ "ndvi",
                            "beta_ag" ~ "agriculture", 
                            "beta_pop" ~ "human pop density", 
                            "beta_impervious" ~ "impervious cover", 
                            "beta_habitat" ~ "habitat")) |> 
  mutate(covar = factor(covar, levels = c("agriculture", "habitat", "impervious cover", "human pop density", "temperature", "ndvi")))

ggplot(mcmc.summary.covar, 
       aes(x = mean, y = covar, color = species))+
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = `2.5%`, xmax = `97.5%`, height = 0.1), linewidth = 1) +
  # Reverse the order of y-axis labels 
  scale_y_discrete(limits = rev)+
  scale_color_manual(values =  c( "#6986a9",  "#774e4e")) +
  xlab("Coefficient") +
  # labs(title = "Covariate coefficients")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  theme_classic() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 20, face = 'bold', margin = margin(0.6, 0, 0, 0, unit = "cm")),
        axis.text = element_text(size = 18),
        legend.title = element_blank(), 
        legend.position = "top",
        legend.text = element_text(size = 18), 
        panel.grid.major.y = element_line(color = "gray70"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        plot.title.position = "plot",
        plot.margin = margin(0, 1.5, 1.5, 1.5, unit = "cm")) -> p1

p1

mcmc.summary.city <- mcmc.summary |> 
  filter(coefficient %in% c("beta_city[1]", "beta_city[2]", "beta_city[3]", "beta_city[4]", "beta_city[5]", "beta_city[6]", "beta_city[7]", "beta_city[8]","beta_city[9]")) |> 
  mutate(city = case_match(coefficient, 
                           "beta_city[1]" ~ "Boston MA", 
                           "beta_city[2]" ~ "Chicago IL", 
                           "beta_city[3]" ~ "Des Moines IA", 
                           "beta_city[4]" ~ "Indianapolis IN", 
                           "beta_city[5]" ~ "Iowa City IA", 
                           "beta_city[6]" ~ "Jackson MS", 
                           "beta_city[7]" ~ "Little Rock AR", 
                           "beta_city[8]" ~ "Washington DC",
                           "beta_city[9]" ~ "Wilmington DE")) |> 
  # organized by population size 
  mutate(city = factor(city, levels = c("Chicago IL", "Indianapolis IN", 
                                        "Washington DC", "Boston MA", 
                                        "Des Moines IA", "Little Rock AR", 
                                        "Jackson MS", "Iowa City IA", "Wilmington DE")))

# organized by density 
# mutate(city = factor(city, levels = c("Boston MA", "Chicago IL",  "Washington DC", 
#                                       "Wilmington DE", "Iowa City IA", "Des Moines IA", 
#                                       "Indianapolis IN","Little Rock AR", 
#                                       "Jackson MS")))

ggplot(mcmc.summary.city, 
       aes(x = exp(mean), y = city, color = species))+
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = exp(`2.5%`), xmax = exp(`97.5%`), height = 0.1), linewidth = 1) +
  scale_y_discrete(limits = rev)+
  # facet_wrap(~species, scales = "free") +
  scale_color_manual(values =  c( "#6986a9",  "#774e4e")) +
  xlab("Mean relative activity") +
  ggh4x::facetted_pos_scales(y = list(COL == 2 ~ scale_y_discrete(guide = 'none')))+
  lemon::facet_rep_grid(~species, scales = "free")+
  # labs(title = "Coefficient values by city") +
  theme_classic() +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 20, face = 'bold', margin = margin(0.6, 0, 0, 0, unit = "cm")),
        axis.text = element_text(size = 18),
        axis.text.y = element_text(margin = margin(0, 0.3, 0, 0, 'cm')),
        legend.title = element_blank(), 
        legend.position = "top",
        legend.text = element_text(size = 18), 
        panel.grid.major.y = element_line(color = "gray70"), 
        strip.text = element_blank(), 
        plot.title.position = "plot",
        plot.margin = margin(0, 1.5, 1.5, 1.5, unit = "cm")) -> p2

# now add the title
title <- cowplot::ggdraw() + 
  cowplot::draw_label(format(Sys.Date(), "%A %B %d %Y"),
                      fontface = 'bold',
                      x = 0,
                      hjust = 0) +
  theme(# add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 400))


cowplot::plot_grid(title, p1, p2, 
                   nrow = 3,
                   rel_heights = c(0.05, 0.8, 0.8)) -> fp

ggsave(fp, 
       filename = "./spatial_analysis/model_plots/coefficient_plots.pdf", 
       dpi = 300, 
       width = 8, 
       height = 11, 
       units = c("in"))

# save plot 1 as jpeg 
ggsave(p1, 
       filename = "./spatial_analysis/model_plots/covariate_coefficient.jpeg", 
       dpi = 300, 
       width = 12, 
       height = 8)

# save plot 2 as jpeg 
ggsave(p2, 
       filename = "./spatial_analysis/model_plots/city_coefficient.jpeg", 
       dpi = 300, 
       width = 12, 
       height = 8)


# Predictions: Global human model ------------------------------------------------------------------
## Spatial covariates -----------------------------------
# You don't want to make predictions from the mean of each posterior.
# Rather, you want to do the predictions across the posterior and then calculate
# the mean (or median). 
colnames(mcmc.output$chain1)

cov_order <- c("Agriculture" = "ag_prop", 
               "Habitat" = "habitat_prop", 
               "Impervious" = "imp_mean", 
               "Human population" = "pop_dens")

# Create a data frame to put the min and max values of each covariate
# 2 columns (min, max) and the number of rows corresponds with the number of covariates 
# Make ranges for plotting
# This is for the x axis so they don't need to be scaled
my_ranges <- cov_mat |>
  select( ag_prop, habitat_prop, imp_mean, pop_dens) |> 
  pivot_longer(everything(), 
               names_to = 'variable', 
               values_to = 'values') |> 
  group_by(variable) |> 
  summarise(min = min(values), 
            max = max(values)) 

# create a sequence of 200 values per covariate  
seq_x <- my_ranges %>%
  rowwise() %>%
  mutate(x = list(seq(min, max, length.out = 200))) %>%
  unnest(x) |> 
  select(variable, x) |> 
  mutate(variable = case_match(variable, 
                               "ag_prop" ~ "x_ag", 
                               "habitat_prop" ~  "x_habitat", 
                               "imp_mean" ~ "x_imp", 
                               "pop_dens" ~ "x_pop"))

# Function to scale all my different covariates 
## City values to scale 
covs_means <- cov_mat |>
  select(city, ag_prop, habitat_prop, imp_mean, pop_dens) |> 
  summarise(across(ag_prop:pop_dens, ~ mean(.x, na.rm=TRUE), .names = "{.col}_mean")) |> 
  ungroup() |> 
  pivot_longer(cols = everything(), 
               names_to = 'name', 
               values_to = 'mean') |> 
  mutate(variable =  c("x_ag", "x_habitat", "x_imp", "x_pop"))

global_sd <- cov_mat |> 
  select(city, ag_prop, habitat_prop, imp_mean, pop_dens) |> 
  summarise(across(ag_prop:pop_dens, ~ sd(.x, na.rm=TRUE), .names = "{.col}_global_sd")) |> 
  pivot_longer(cols = everything(), 
               names_to = 'name', 
               values_to = 'sd') |> 
  mutate(variable =  c("x_ag", "x_habitat", "x_imp", "x_pop"))

seq_x1 <- left_join(covs_means, global_sd, by = join_by('variable')) |> 
  select(variable, mean, sd)

seq_x <- left_join(seq_x, seq_x1, by = join_by('variable'))

# Create a matrix with x sequence for each covariate (column)
seq_x_mat <- seq_x |> 
  select(variable, x) |>
  mutate(id = rep(seq(1,200), 4)) |> 
  pivot_wider(id_cols = id, 
              names_from = variable, 
              values_from = x) |> 
  select(!id) |> 
  as.matrix()

# Create a matrix with x-scaled for each covariate 
x_scale<- seq_x |> 
  mutate(x_scale = (x - mean)/sd) |> 
  mutate(id = rep(seq(1, 200, by = 1), times = 4)) |>
  pivot_wider(id_cols = id, 
              names_from = variable, 
              values_from = x_scale) |> 
  select(x_ag:x_pop)

# put scaled x in a matrix 
x_mat <- as.matrix(x_scale)

# put betas in a matrix and take a random sample of 10K
beta_mat <- do.call(rbind, mcmc.output)
set.seed(787)
sim_subset <- beta_mat[sample(1:nrow(beta_mat), 10000),]

# extract only the covariate betas and mu_city
cov_beta_mat <- sim_subset[, c("mu_city", 
                               "beta_ag", "beta_habitat", "beta_impervious", "beta_pop")]

# For the offset, estimate the mean of the entire deer_jmat matrix 
# Except the first column which is siteID
human_jmat_mean <- data.frame(human_jmat[-1]) |> 
  summarise_all(list(~mean(., na.rm = TRUE))) |> 
  rowMeans()

human_jmat_mean


# Make a list to put in the predicted results of each covariate = 4
pred_results <- vector("list", length = 4)
names(pred_results) <- c("ag", "habitat", "impervious", "pop_density")

# Now we start filling it in
for(i in seq_along(pred_results)){
  # make a temporary design matrix
  tmp_dm <- matrix(
    0,
    ncol = ncol(x_mat),
    nrow = nrow(x_mat)
  )
  
  # add in the values for that specific covariate
  tmp_dm[,i] <- x_mat[,i]
  
  # add 1 for the intercept
  tmp_dm <- cbind(1, tmp_dm)
  
  # make predictions
  pred_results[[i]] <- cov_beta_mat %*% t(tmp_dm)
  # put back on real scale using exp
  pred_results[[i]] <- exp(pred_results[[i]] + log(human_jmat_mean)) ## Check notation of log here !!!!!!
  # and get the 95% CI for each covariate
  pred_results[[i]] <- apply(
    pred_results[[i]],
    2,
    quantile,
    probs = c(0.025,0.5,0.975)
  )
  # transpose so it's long, not wide
  pred_results[[i]] <- data.frame( 
    t(pred_results[[i]])
  )
  
  # Remove X at the beginning of credible intervals columns and substitute with CI_ 
  names(pred_results[[i]]) <- names(pred_results[[i]]) |> 
    stringr::str_replace("^X", "CI_") |> 
    stringr::str_replace("\\.$", "")
  
  # labels to add as x-axis titles 
  nms <- factor(c("Agriculture", 
                  "Habitat", 
                  "Impervious", 
                  "Human population"), 
                levels = c("Agriculture", 
                           "Habitat", 
                           "Impervious", 
                           "Human population"))
  
  x_covs <- c("x_ag", "x_habitat", "x_imp", "x_pop")
  
  # Create new variables for labeling 
  pred_results[[i]] <- pred_results[[i]] |>
    mutate(x = seq_x_mat[,i],
           variable = nms[i])
  
}

pred_results_df <- do.call(rbind, pred_results)

pred_results_df <- pred_results_df |> 
  mutate(species = "human")

colors_pred_human <- c('#2081f9','#9eb3c2' )

colors_pred2 <- c(
  '#4d194d', "#2a9d8f",'#312244', '#065a82', "#86bbbd", '#9eb3c2', 
  "#264653","#e9c46a","#f4a261","#e76f51"
)

ggplot(pred_results_df)+
  geom_ribbon(aes(x = x, 
                  ymin = CI_2.5, 
                  ymax = CI_97.5), 
              fill = colors_pred_human[2], 
              alpha = 0.3) +
  geom_line(aes(x = x, 
                y = CI_50), 
            color = colors_pred_human[1], 
            linewidth = 1.2)+
  labs(x = "", 
       y = "Human activity") +
  facet_wrap(~variable, 
             scales = "free_x", 
             nrow=2, 
             strip.position = "bottom",
             labeller = as_labeller(nms)) +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(size = 16, face = 'bold'),
        strip.placement = "outside",
        axis.title = element_text(size = 16, face = 'bold'), 
        axis.text = element_text(size = 14)) -> global_plot_human

global_plot_human

ggsave(plot = global_plot_human,
       filename = paste0('./spatial_analysis/model_plots/', 'global_plot_human', '.jpeg'),
       dpi = 300,
       width = 10,
       height = 10,
       units = 'in')

# Predictions: Global deer model -----------------------------------------------
## Spatial covariates ----------------------------------------------------------
# You don't want to make predictions from the mean of each posterior.
# Rather, you want to do the predictions across the posterior and then calculate
# the mean (or median). 
colnames(mcmc.output.deer$chain1)

cov_order <- c("Agriculture" = "ag_prop", 
               "Habitat" = "habitat_prop", 
               "Impervious" = "imp_mean", 
               "Human population" = "pop_dens")

# Create a data frame to put the min and max values of each covariate
# 2 columns (min, max) and the number of rows corresponds with the number of covariates 
# Make ranges for plotting
# This is for the x axis so they don't need to be scaled
my_ranges <- cov_mat |>
  select( ag_prop, habitat_prop, imp_mean, pop_dens) |> 
  pivot_longer(everything(), 
               names_to = 'variable', 
               values_to = 'values') |> 
  group_by(variable) |> 
  summarise(min = min(values), 
            max = max(values)) 

# create a sequence of 200 values per covariate  
seq_x <- my_ranges %>%
  rowwise() %>%
  mutate(x = list(seq(min, max, length.out = 200))) %>%
  unnest(x) |> 
  select(variable, x) |> 
  mutate(variable = case_match(variable, 
                               "ag_prop" ~ "x_ag", 
                               "habitat_prop" ~  "x_habitat", 
                               "imp_mean" ~ "x_imp", 
                               "pop_dens" ~ "x_pop"))

# Function to scale all my different covariates 
## City values to scale 
covs_means <- cov_mat |>
  select(city, ag_prop, habitat_prop, imp_mean, pop_dens) |> 
  summarise(across(ag_prop:pop_dens, ~ mean(.x, na.rm=TRUE), .names = "{.col}_mean")) |> 
  ungroup() |> 
  pivot_longer(cols = everything(), 
               names_to = 'name', 
               values_to = 'mean') |> 
  mutate(variable =  c("x_ag", "x_habitat", "x_imp", "x_pop"))

global_sd <- cov_mat |> 
  select(city, ag_prop, habitat_prop, imp_mean, pop_dens) |> 
  summarise(across(ag_prop:pop_dens, ~ sd(.x, na.rm=TRUE), .names = "{.col}_global_sd")) |> 
  pivot_longer(cols = everything(), 
               names_to = 'name', 
               values_to = 'sd') |> 
  mutate(variable =  c("x_ag", "x_habitat", "x_imp", "x_pop"))

seq_x1 <- left_join(covs_means, global_sd, by = join_by('variable')) |> 
  select(variable, mean, sd)

seq_x <- left_join(seq_x, seq_x1, by = join_by('variable'))

# Create a matrix with x sequence for each covariate (column)
seq_x_mat <- seq_x |> 
  select(variable, x) |>
  mutate(id = rep(seq(1,200), 4)) |> 
  pivot_wider(id_cols = id, 
              names_from = variable, 
              values_from = x) |> 
  select(!id) |> 
  as.matrix()

# Create a matrix with x-scaled for each covariate 
x_scale<- seq_x |> 
  mutate(x_scale = (x - mean)/sd) |> 
  mutate(id = rep(seq(1, 200, by = 1), times = 4)) |>
  pivot_wider(id_cols = id, 
              names_from = variable, 
              values_from = x_scale) |> 
  select(x_ag:x_pop)

# put scaled x in a matrix 
x_mat <- as.matrix(x_scale)

# put betas in a matrix and take a random sample of 10K
beta_mat_deer <- do.call(rbind, mcmc.output.deer)
set.seed(787)
sim_subset <- beta_mat_deer[sample(1:nrow(beta_mat_deer), 10000),]

# extract only the covariate betas and mu_city
cov_beta_mat_deer <- sim_subset[, c("mu_city", 
                                    "beta_ag", "beta_habitat", "beta_impervious", "beta_pop")]

# For the offset, estimate the mean of the entire deer_jmat matrix 
# Except the first column which is siteID
deer_jmat_mean <- data.frame(deer_jmat[-1]) |> 
  summarise_all(mean) |> 
  rowMeans()

# Make a list to put in the predicted results of each covariate = 4
pred_results_deer <- vector("list", length = 4)
names(pred_results_deer) <- c("ag", "habitat", "impervious", "pop_density")

# Now we start filling it in
for(i in seq_along(pred_results_deer)){
  # make a temporary design matrix
  tmp_dm <- matrix(
    0,
    ncol = ncol(x_mat),
    nrow = nrow(x_mat)
  )
  
  # add in the values for that specific covariate
  tmp_dm[,i] <- x_mat[,i]
  
  # add 1 for the intercept
  tmp_dm <- cbind(1, tmp_dm)
  
  # make predictions
  pred_results_deer[[i]] <- cov_beta_mat_deer %*% t(tmp_dm)
  # put back on real scale using exp
  pred_results_deer[[i]] <- exp(pred_results_deer[[i]] + log(deer_jmat_mean)) ## Check notation of log here !!!!!!
  # and get the 95% CI for each covariate
  pred_results_deer[[i]] <- apply(
    pred_results_deer[[i]],
    2,
    quantile,
    probs = c(0.025,0.5,0.975)
  )
  # transpose so it's long, not wide
  pred_results_deer[[i]] <- data.frame( 
    t(pred_results_deer[[i]])
  )
  
  # Remove X at the beginning of credible intervals columns and substitute with CI_ 
  names(pred_results_deer[[i]]) <- names(pred_results_deer[[i]]) |> 
    stringr::str_replace("^X", "CI_") |> 
    stringr::str_replace("\\.$", "")
  
  # labels to add as x-axis titles 
  nms <- factor(c("Agriculture", 
                  "Habitat", 
                  "Impervious", 
                  "Human population"), 
                levels = c("Agriculture", 
                           "Habitat", 
                           "Impervious", 
                           "Human population"))
  
  x_covs <- c("x_ag", "x_habitat", "x_imp", "x_pop")
  
  # Create new variables for labeling 
  pred_results_deer[[i]] <- pred_results_deer[[i]] |>
    mutate(x = seq_x_mat[,i],
           variable = nms[i])
  
}

pred_results_deer_df <- do.call(rbind, pred_results_deer)
pred_results_deer_df <- pred_results_deer_df |> 
  mutate(species = "deer")


colors_pred_deer <- c("#A65141", "#d8a69b")

colors_pred_deer_old <- c("#FF5733", "#f4a261", "#FFBF00")

ggplot(pred_results_deer_df)+
  geom_ribbon(aes(x = x, 
                  ymin = CI_2.5, 
                  ymax = CI_97.5), 
              fill = colors_pred_deer[2], 
              alpha = 0.3) +
  geom_line(aes(x = x, 
                y = CI_50), 
            color = colors_pred_deer[1], 
            linewidth = 1.2)+
  labs(x = "", 
       y = "Deer activity") +
  facet_wrap(~variable, 
             scales = "free_x", 
             nrow=2,
             strip.position = "bottom",
             labeller = as_labeller(nms)) +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_text(size = 16, face = 'bold'),
        strip.placement = "outside",
        axis.title = element_text(size = 16, face = 'bold'), 
        axis.text = element_text(size = 14)) -> global_plot_deer

global_plot_deer


ggsave(plot = global_plot_deer,
       filename = paste0('./spatial_analysis/model_plots/', 'global_plot_deer', '.jpeg'),
       dpi = 300,
       width = 10,
       height = 10,
       units = 'in')




# Predictions: City human model ------------------------------------------------------------------
## Spatial covariates -----------------------------------

# You don't want to make predictions from the mean of each posterior.
# Rather, you want to do the predictions across the posterior and then calculate
# the mean (or median). 
colnames(mcmc.output$chain1)

city_order <- c("boma" = "boma", #1
                "chil" = "chil", #2
                "deio" = "deio", #3
                "inin" = "inin", #4
                "ioio" = "ioio", #5
                "jams" = "jams", #6
                "lrar" = "lrar", #7
                "naca" = "naca", #8
                "wide" = "wide") #9

cov_order <- c("Agriculture" = "ag_prop", 
               "Habitat" = "habitat_prop", 
               "Impervious" = "imp_mean", 
               "Human population" = "pop_dens")

# Estimate ranges per city 
ranges_city <- cov_mat |> 
  select(city, ag_prop, habitat_prop, imp_mean, pop_dens) |> 
  group_by(city) |> 
  summarise(across(ag_prop:pop_dens, list(min = min, 
                                          max = max), 
                   .names = "{.col}_{.fn}"))

# create a sequence of 200 values per city 
seq_city <- ranges_city |> 
  mutate(x_ag = purrr::map2(ag_prop_min, ag_prop_max, seq, length.out = 200), 
         x_habitat = purrr::map2(habitat_prop_min, habitat_prop_max, seq, length.out = 200), 
         x_imp = purrr::map2(imp_mean_min, imp_mean_max, seq, length.out = 200), 
         x_pop = purrr::map2(pop_dens_min, pop_dens_max, seq, length.out = 200)) |> 
  unnest(cols = c(x_ag, x_habitat, x_imp, x_pop)) |> 
  select(city, x_ag, x_habitat, x_imp, x_pop)

# Function to scale all my different x that 
# resembles exactly the way I scaled my covariates 
# City mean values to scale 
city_means <- cov_mat |>
  select(city, ag_prop, habitat_prop, imp_mean, pop_dens) |> 
  group_by(city) |> 
  summarise(across(ag_prop:pop_dens, ~ mean(.x, na.rm=TRUE), .names = "{.col}_city_mean")) |> 
  ungroup() 

# global standar deviation
global_sd <- cov_mat |> 
  select(city, ag_prop, habitat_prop, imp_mean, pop_dens) |> 
  summarise(across(ag_prop:pop_dens, ~ sd(.x, na.rm=TRUE), .names = "{.col}_global_sd"))

# Join means with x data frame 
seq_means <- left_join(seq_city, city_means, by = join_by("city"))

# Scale all x 
# This is my scaled x data frame for cities 
x_scale <- seq_means |> 
  mutate(x_ag_scale = (x_ag - ag_prop_city_mean)/global_sd$ag_prop_global_sd, 
         x_habitat_scale = (x_habitat - habitat_prop_city_mean)/global_sd$habitat_prop_global_sd, 
         x_imp_scale = (x_imp - imp_mean_city_mean)/global_sd$imp_mean_global_sd,
         x_pop_scale = (x_pop - pop_dens_city_mean)/global_sd$pop_dens_global_sd) |> 
  select(city, x_ag, x_ag_scale, x_habitat, x_habitat_scale, x_imp, x_imp_scale, x_pop, x_pop_scale)

# divide x_scale by city and put in a list 
x_scale_list <- split(x_scale, f = x_scale$city)
# put scaled x in a matrix 
x_mat_list <- map(
  .x = x_scale_list, 
  .f = function(df){
    df |> 
      select(ends_with("_scale")) |> 
      as.matrix()
  }
) 

# put betas in a matrix and take a random sample of 10K
beta_mat <- do.call(rbind, mcmc.output)
set.seed(787)
sim_subset <- beta_mat[sample(1:nrow(beta_mat), 10000),]
# extract only the covariate betas and mu_city
cov_beta_mat <- sim_subset[, c("beta_city[1]", "beta_city[2]","beta_city[3]", "beta_city[4]",
                               "beta_city[5]", "beta_city[6]","beta_city[7]", "beta_city[8]",
                               "beta_city[9]",
                               "beta_ag", "beta_habitat", "beta_impervious", "beta_pop")]

# Create a list of matrices with the beta for each city and all the covariate betas
beta_city_list <- list()
for(i in seq_along(city_order)){
  beta_city_list[[i]] <- cov_beta_mat[, c(paste0("beta_city[", i, "]"),  
                                          "beta_ag", "beta_habitat", "beta_impervious", "beta_pop")]
}

names(beta_city_list) <- city_order

# Function to repeat each vector twice in the list
repeat_vectors <- function(lst) {
  unlist(lapply(lst, function(x) list(x, x, x, x)), recursive = FALSE)
}

beta_city_list <- repeat_vectors(beta_city_list)

# For the offset, estimate the mean of the entire human_jmat matrix 
human_jmat_mean <- data.frame(human_jmat[-1]) |> 
  summarise_all(list(~mean(., na.rm = TRUE))) |> 
  rowMeans()

# vector with pretty city names 
fcity <- cov_mat |> 
  distinct(fcity) |> 
  mutate(fcity = purrr::map(fcity, ~rep(.x, 200))) %>%
  unnest(fcity)

fcity_list <- split(fcity, fcity$fcity)

# Make a list to put in the predicted results of each covariate = 4
pred_results <- vector("list", length = 9)
names(pred_results) <- city_order
for(i in seq_along(pred_results)){
  pred_results[[i]] <- list(
    ag = NULL, 
    habitat = NULL, 
    impervious = NULL, 
    pop_density = NULL)
}

# for(i in seq_along(pred_results)){
#   pred_results[[i]] <- list(
#     ag = data.frame(x=c(1:10), y = c(10:19)), 
#     habitat = data.frame(x=c(10:19), y = c(100:109)), 
#     impervious = data.frame(x=c(100:109), y = c(1000:1009)), 
#     pop_density = data.frame(x=c(1000:1009), y = c(10000:10009))
#   )
# }

# Now we start filling it in
for(i in seq_along(beta_city_list)){
  for(j in seq_along(pred_results)){
    # make a temporary design matrix
    tmp_dm <- matrix(
      0,
      ncol = ncol(x_mat_list[[i]]), # 4, covariates
      nrow = nrow(x_mat_list[[i]])  # 200 rows (number of x sequence)
    )
  
    # add in the values for that specific covariate
    # the other columns remain 0 to keep the other covariates constant
    tmp_dm[,j] <- x_mat_list[[i]][,j] ## This is giving error because it needs to loop to 4
    
    # add 1 for the intercept
    tmp_dm <- cbind(1, tmp_dm)
  
  # make predictions
  pred_results[[i]][[j]] <- beta_city_list[[i]] %*% t(tmp_dm)
  # put back on real scale using exp
  pred_results[[i]][[j]] <- exp(pred_results[[i]][[j]] + log(human_jmat_mean)) ## Check notation of log here !!!!!!
  # and get the 95% CI for each covariate
  pred_results[[i]][[j]] <- apply(
    pred_results[[i]][[j]],
    2,
    quantile,
    probs = c(0.025,0.5,0.975)
  )
  # transpose so it's long, not wide
  pred_results[[i]][[j]] <- data.frame(
    t(pred_results[[i]][[j]])
  )

  # # Remove X at the beginning of credible intervals columns and substitute with CI_ 
  # names(pred_results[[j]]) <- names(pred_results[[j]]) |> 
  #   stringr::str_replace("^X", "CI_") |> 
  #   stringr::str_replace("\\.$", "")
  # 
  #   # labels to add as x-axis titles 
  # nms <- factor(c("Agriculture", 
  #                 "Habitat", 
  #                 "Impervious", 
  #                 "Human population"), 
  #               levels = c("Agriculture", 
  #                          "Habitat", 
  #                          "Impervious", 
  #                          "Human population"))
  # 
  # x_covs <- c("x_ag", "x_habitat", "x_imp", "x_pop")
  
  # Rename variable column 
  # pred_results[[j]] <- pred_results[[j]] |> 
  #   rename(x = x_covs[j]) |> 
  #   mutate(variable = nms[j])
  
  }
}



colors_pred <- c('#2081f9', '#f99820')

colors_pred2 <- c(
  '#4d194d', "#2a9d8f",'#312244', '#065a82', "#86bbbd", '#9eb3c2', 
  "#264653","#e9c46a","#f4a261","#e76f51"
)

for(i in seq_along(pred_results)){
  
  plot <- ggplot(pred_results[[i]])+
    geom_line(aes(x = x, 
                  y = CI_50, 
                  color = city), 
              size = 1.2)+
    geom_ribbon(aes(x = x, 
                    ymin = CI_2.5, 
                    ymax = CI_97.5, 
                    fill = city), 
                alpha = 0.4) +
    labs(x = pred_results[[i]]$variable,
         y = "Estimate") +
    facet_wrap(~fcity, scales = "free_x", nrow=2) +
    scale_color_manual(values = colors_pred) +
    scale_fill_manual(values = colors_pred) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
    theme_classic() +
    theme(legend.position = "none",
          strip.background = element_blank(), 
          strip.text = element_text(size = 16, face = 'bold'),
          axis.title = element_text(size = 16, face = 'bold'), 
          axis.text = element_text(size = 14),
          panel.grid.major.y = element_line(linetype="solid", # add grid line
                                            color = "grey",
                                            size =0.1),
          axis.line.y = element_blank(), 
          axis.line.x = element_line(color = 'gray30', size = 1),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(color =  'gray30', size =  1), 
          axis.ticks.length.x = unit(0.05, units = 'in'), 
          axis.line.x.bottom = element_line(lineend = "square"))
  
  print(plot)
  
  ggsave(plot = plot, 
         filename = paste0('./model_plots/city_', x_covs[i], '.jpeg'), 
         dpi = 300, 
         width = 20, 
         height = 8, 
         units = 'in')
}

# Predictions: City deer model ------------------------------------------------------------------
## Spatial covariates -----------------------------------
# You don't want to make predictions from the mean of each posterior.
# Rather, you want to do the predictions across the posterior and then calculate
# the mean (or median). 
colnames(mcmc.output.deer$chain1)

# Make a list to put in the predicted results of each covariate = 4
pred_results_deer <- vector("list", length = 4)

# put betas in a matrix and take a random sample of 10K
beta_mat_deer <- do.call(rbind, mcmc.output.deer)
set.seed(787)
sim_subset_deer <- beta_mat_deer[sample(1:nrow(beta_mat_deer), 10000),]
# extract only the covariate betas and mu_city
cov_beta_mat_deer <- sim_subset_deer[, c("mu_city", "beta_ag", "beta_habitat", "beta_impervious", "beta_pop")]

# For the offset, estimate the mean of the entire human_jmat matrix 
deer_jmat_mean <- data.frame(deer_jmat)[-1] |> 
  summarise_all(mean) |> 
  rowMeans()

# Now we start filling it in
for(i in seq_along(pred_results_deer)){
  # make a temporary design matrix
  tmp_dm <- matrix(
    0,
    ncol = ncol(x_mat),
    nrow = nrow(x_mat)
  )
  
  # add in the values for that specific covariate
  tmp_dm[,i] <- x_mat[,i]
  
  # add 1 for the intercept
  tmp_dm <- cbind(1, tmp_dm)
  
  # make predictions
  pred_results_deer[[i]] <- cov_beta_mat_deer %*% t(tmp_dm)
  # put back on real scale using exp
  pred_results_deer[[i]] <- exp(pred_results_deer[[i]] + log(deer_jmat_mean)) ## Check notation of log here !!!!!!
  # and get the 95% CI for each covariate
  pred_results_deer[[i]] <- apply(
    pred_results_deer[[i]],
    2,
    quantile,
    probs = c(0.025,0.5,0.975)
  )
  # transpose so it's long, not wide
  pred_results_deer[[i]] <- data.frame(
    city = seq_city[, 1], 
    fcity = fcity, 
    covariate = seq_city[, 1+i], 
    t(pred_results_deer[[i]])
  )
  
  # Remove X at the beginning of credible intervals columns and substitute with CI_ 
  names(pred_results_deer[[i]]) <- names(pred_results_deer[[i]]) |> 
    stringr::str_replace("^X", "CI_") |> 
    stringr::str_replace("\\.$", "")
  
  # labels to add as x-axis titles 
  nms <- factor(c("Agriculture", 
                  "Habitat", 
                  "Impervious", 
                  "Human population"), 
                levels = c("Agriculture", 
                           "Habitat", 
                           "Impervious", 
                           "Human population"))
  
  x_covs <- c("x_ag", "x_habitat", "x_imp", "x_pop")
  
  # Rename variable column 
  pred_results_deer[[i]] <- pred_results_deer[[i]] |> 
    rename(x = x_covs[i]) |> 
    mutate(variable = nms[i])
  
}

names(pred_results) <- c("ag", "habitat", "impervious", "pop_density")

colors_pred <- c(
  '#4d194d', "#2a9d8f",'#312244', '#065a82', "#86bbbd", '#9eb3c2', 
  "#264653","#e9c46a","#f4a261","#e76f51"
)

for(i in seq_along(pred_results_deer)){
  
  plot <- ggplot(pred_results_deer[[i]])+
    geom_line(aes(x = x, 
                  y = CI_50, 
                  color = city), 
              size = 1.2)+
    geom_ribbon(aes(x = x, 
                    ymin = CI_2.5, 
                    ymax = CI_97.5, 
                    fill = city), 
                alpha = 0.4) +
    labs(x = pred_results_deer[[i]]$variable,
         y = "Estimate") +
    facet_wrap(~fcity, scales = "free_x", nrow=2) +
    scale_color_manual(values = colors_pred) +
    scale_fill_manual(values = colors_pred) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
    theme_classic() +
    theme(legend.position = "none",
          strip.background = element_blank(), 
          strip.text = element_text(size = 16, face = 'bold'),
          axis.title = element_text(size = 16, face = 'bold'), 
          axis.text = element_text(size = 14),
          panel.grid.major.y = element_line(linetype="solid", # add grid line
                                            color = "grey",
                                            size =0.1),
          axis.line.y = element_blank(), 
          axis.line.x = element_line(color = 'gray30', size = 1),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(color =  'gray30', size =  1), 
          axis.ticks.length.x = unit(0.05, units = 'in'), 
          axis.line.x.bottom = element_line(lineend = "square"))
  
  print(plot)
  
  ggsave(plot = plot,
         filename = paste0('./model_plots/city_deer_', x_covs[i], '.jpeg'),
         dpi = 300,
         width = 20,
         height = 8,
         units = 'in')
}

