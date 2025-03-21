## Bayesian Poisson model with random effect and an offset term
# i = sites 
# j = seasons
# k = cities 

## Data required ---------------------------------------------------------------
## indices ---------------------------------------------------------------------
# nsites: scalar, number of unique sites across all seasons, 437
# nseasons: scalar, number of seasons sampled (e.g., AP19 is April 2019), 17.
# ncity: scalar, number of cities included in the study, 9 
# city_vec: vector of length i that denotes what city each site belongs to (e.g, 1,1,1,1... for boma)
# factor of cities: autx, boma, chil, deio, inin, ioio, jams, lrar, naca, wide.
# nbeta: scalar, number of covariates, 5
# offset: deer_jmat 


## matrices ------------------------------------------------------------------
# deer_jmat: number of days cameras were active per sites x seasonsyear, 437x17
# deer_detections: number of deer detections per sites x seasonsyear, 437x17
# habitat: as.vector(df$habitat)
# 

# Define the model
code <- nimbleCode({
  # priors for fixed effects
  beta_habitat ~ dnorm(0, 10)
  beta_ag ~ dnorm(0, 10)
  beta_impervious ~ dnorm(0, 10)
  beta_pop ~ dnorm(0, 10)
  beta_temp ~ dnorm(0, 10)
  beta_ndvi ~ dnorm(0, 10)
  
  # Prior for city random effect
  for (k in 1:ncity) {
    beta_city[k] ~ dnorm(mu_city, sigma_city) # will give each city an average intercept; partial pooling
  }
  
  # Prior for precision of city random effect
  mu_city ~ dnorm(0, 10) # average intercept of all the cities
  sigma_city ~ dgamma(1, 1) 
  
  # Poisson likelihood
  for(i in 1:nsites){
    for(j in 1:nseasons){
      
      count[i,j] ~ dpois(lambda[i,j])
      
      log(lambda[i,j]) <- beta_city[city_vec[i]] + # random effect 
        log(offset[i,j]) + # offset term
        beta_habitat * habitat[i] + 
        beta_ag * ag[i] +
        beta_impervious * impervious[i] +
        beta_pop * pop[i] +
        beta_temp * temp[i, j]+
        beta_ndvi * ndvi[i, j]
    }
  }
})



