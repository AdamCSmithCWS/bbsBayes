library(bbsBayes)
library(nimble)
library(spdep)
library(sf)
library(tidyverse)

dbstrat = stratify(by = "latlong")
jags_data = prepare_jags_data(dbstrat,species_to_run = "Wood Thrush",model = "slope",min_year = 2007,max_year = 2018,min_n_routes = 1)


model_to_file(model = "slope",filename = "tmp/slope_model.R")

map <- sf::read_sf(dsn = "inst/maps",
                   layer = "BBS_LatLong_strata",
                   quiet = TRUE)




prepdat = get_prepared_data(jags_data)
strats <- unique(prepdat[,c("Stratum","Stratum_Factored")])
strats$ST_12 <- strats$Stratum
strats <- strats[order(strats$Stratum_Factored),]

map_sel <- right_join(map,strats,by = "ST_12") %>%
  arrange(Stratum_Factored)


nb_db = poly2nb(map_sel,row.names = "ST_12")
nb_info = nb2WB(nb_db)

jags_data[["adj"]] <- nb_info$adj
jags_data[["weights"]] <- nb_info$weights
jags_data[["num"]] <- nb_info$num
jags_data[["nNeighbours"]] <- length(nb_info$adj)

slope_nim <- readBUGSmodel(model = "tmp/slope_CAR_model.R",data = jags_data,
                           inits = list(BETA = 0, beta = rnorm(jags_data$nstrata,0,0.001)))
cSlope_nim <- compileNimble(slope_nim)
slope_MCMC <- buildMCMC(cslope_nim)
Cslope_MCMC <- compileNimble(slope_MCMC)
results <- runMCMC(Cslope_MCMC,niter = 100)

