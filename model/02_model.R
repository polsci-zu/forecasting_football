#-----------------------------------------------------------------
badr::remove_libraries()
badr::require_quite(c(
  "plyr","tidyverse","rvest","magrittr","stringr","jsonlite","rstan")
)

source("model/00_function.R")

#-----------------------------------------------------------------

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("model/data.Rdata")

# Teams --------------------------------------------------------------------

data = model_preparation(team_ranking,
                         seasons,
                         whichseasons = c("season16","season17"),
                         maxmatchday = c(22))
names(data)
#--------------------------------------------------------------------------

fit <- stan("model/model_pois_0.04.stan", data=data, chains=4, iter=4000)
#fit <- stan("model_pois_0.06(correlated).stan", data=data, chains=4, iter=400)
print(fit,pars = c("baseline","b_prior","home","skill"))
#save(fit,file = "model/model_pois_BIG.RData")
