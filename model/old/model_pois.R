#-----------------------------------------------------------------
badr::remove_libraries()
badr::require_quite(c(
  "plyr","tidyverse","rvest","magrittr","stringr","jsonlite","rstan")
)
#-----------------------------------------------------------------

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("data.Rdata")
# Teams --------------------------------------------------------------------

#teams <- as.vector (unlist (read.table ("soccerpowerindex.txt", header=FALSE)))
#nteams <- length(teams)

teams <- team_ranking
nteams <- length(teams)

prior_score <- rev(1:nteams)
prior_score <- (prior_score - mean(prior_score))/(2*sd(prior_score))

# Data 
data2017 <- seasons[,c("Team1","Goals1","Team2","Goals2")]
ngames <- nrow(data2017)

team1 <- match (as.vector(data2017[[1]]), teams)
score1 <- as.vector(data2017[[2]])
team2 <- match (as.vector(data2017[[3]]), teams)
score2 <- as.vector(data2017[[4]])

data <- c("nteams","ngames","team1","score1","team2","score2","prior_score")

#--------------------------------------------------------------------------

fit <- stan("model_pois_0.04.stan", data=data, chains=4, iter=4000)
fit <- stan("model_pois_0.06(correlated).stan", data=data, chains=4, iter=400)
print(fit,pars = c("baseline","b_prior","home","skill"))

save(fit,file = "model_pois.Rdata")
save(fit,file = "model_pois.Rdata")
