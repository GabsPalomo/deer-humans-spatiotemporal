library(nimble)
library(readr)
library(dplyr)
library(MCMCvis)
library(kableExtra)

# don't use scientific notation 
options(scipen=999)

## source the Poisson model 
source('./spatial_analysis/poisson_model_01.R')

## data ------------------------------------------------------------------------
## days cameras were active across all sites and seasons
# [-1] is to remove the first column which has site names 
human_jmat <- read_csv('./spatial_analysis/model_matrices/human_jmat.csv')[, -1]
## deer detections across all sites and seasons
# [-1] is to remove the first column which has site names 
human_detections <- read_csv('./spatial_analysis/model_matrices/human_det.csv')[, -1]

## covariates 
cov_mat <- read_csv('./spatial_analysis/model_matrices/cov_mat.csv')

# indices ----------------------------------------------------------------------
nsites <- nrow(human_detections)
nseasons <- ncol(human_detections)
ncity <- length(unique(cov_mat$city))
city_vec <- as.numeric(factor(cov_mat$city))

# matrices --------------------------------------------------------------------
## spatial covariates 
habitat <- cov_mat$scale_habitat_prop
ag <- cov_mat$scale_ag_prop
impervious <- cov_mat$scale_imp_value
pop <- cov_mat$scale_pop_dens
## spatial and temporal covariate
temp_mat <- readr::read_csv('./spatial_analysis/model_matrices/temp_mat_unique_ij.csv')[, -1]
ndvi_mat <- read_csv('./spatial_analysis/model_matrices/UWIN.NDVI.forGabby_2024.07.16.csv')[, 8:23]

# create constant list 
my.constants <- list(
  # covariates 
  habitat = habitat, 
  ag = ag, 
  impervious = impervious, 
  pop = pop, 
  temp = temp_mat,
  ndvi = ndvi_mat,
  # scalars 
  nsites = nsites, 
  nseasons = nseasons, 
  ncity = ncity, 
  city_vec = city_vec
)


# Create data list
my.data <- list(
  count = as.matrix(human_detections),
  offset = as.matrix(human_jmat)
)


# initial values 
my.inits <- function(){
  list(beta_habitat = rnorm(1, 0, 10),
       beta_ag = rnorm(1, 0, 10),
       beta_impervious = rnorm(1, 0, 10),
       beta_pop = rnorm(1, 0, 10),
       beta_temp = rnorm(1, 0, 10),
       beta_ndvi = rnorm(1, 0, 10),
       mu_city = rnorm(1, 0, 10),
       sigma_city = rgamma(1, 1, 1),
       # random effects
       beta_city = rnorm(ncity, 0, 10)
  )
}

parameters.to.save <- c('beta_habitat', 'beta_ag', 'beta_impervious', 'beta_pop', 'beta_temp', 'beta_ndvi',
                        'mu_city', 'sigma_city', 'beta_city'
)

# specify mcmc details 
n.iter <- 310000
n.burnin <- 10000
n.chains <- 4
n.thin <- 8

# number of final samples per chain 
(nfsc <- (n.iter - n.burnin)/n.thin)
# 37 500

# run the mcmc
my_start <- Sys.time()
mcmc.output <-  nimbleMCMC(code = code, 
                           data = my.data,
                           constants = my.constants, 
                           inits = my.inits,
                           monitors = parameters.to.save, 
                           thin = n.thin, 
                           niter = n.iter, 
                           nburnin = n.burnin,
                           nchains = n.chains
                           # progressBar = getNimbleOption("MCMCprogressBar"),
                           # samples = TRUE,
                           # samplesAsCodaMCMC = TRUE,
                           # summary = TRUE
)

my_end <- Sys.time()
(full_time <- difftime(my_end, my_start, units = 'mins'))

# save mcmc.output as rds file 
readr::write_rds(mcmc.output, 
                 paste0('./spatial_analysis/rds_model/human_mcmc_output_01_', 
                        Sys.Date(), '.rds'))

# Let's see the mcmc samples --------------------------------------------------
# explore MCMC outputs 
str(mcmc.output)
# list with iterations for each chain, 4 chains in total 

# dimensions of each chain: samples x parameters
(dim(mcmc.output$chain1))
# 25,000 samples 
# 16 parameters 

# See first 5 rows of each chain 
purrr::map(
  .x = mcmc.output, 
  .f = ~.x[c(1:5), ]
)

# Check for NAs in chains
purrr::map(
  .x = mcmc.output, 
  .f = ~which(is.na(.x))
)


## change the list to a matrix so rows are samples from all the chains and columns are parameters
mcmc.mat <- do.call(rbind, mcmc.output)
dim(mcmc.mat)

# summary 
mcmc.summary <- MCMCvis::MCMCsummary(mcmc.output, 
                                     round = 2, 
                                     # probabilities and median
                                     probs = c(0.025, 0.5, 0.975))
# Put in a kable table so it's exportable 
mcmc.summary |> 
  kbl() |> 
  kable_styling(full_width = FALSE) 

# trace and posterior density plots of survival 
MCMCtrace(mcmc.output, 
          # show effective samples on density plot 
          n.eff = TRUE, 
          # show individual density plots for each chain
          ind = TRUE, 
          # show Rhat value on density plot 
          Rhat = TRUE,
          pdf = TRUE, 
          filename = paste0('./spatial_analysis/model_results/human_mcmc_output_01_', Sys.Date(), '.pdf'))

# END --------------------------------------------------------------------------