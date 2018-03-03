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
data2017 <- season17_sparse[,c("Team1","Goals1","Team2","Goals2")]
ngames <- nrow (data2017)

team1 <- match (as.vector(data2017[[1]]), teams)
score1 <- as.vector(data2017[[2]])
team2 <- match (as.vector(data2017[[3]]), teams)
score2 <- as.vector(data2017[[4]])

df <- 7

data <- c("nteams","ngames","team1","score1","team2","score2","prior_score","df")

#--------------------------------------------------------------------------

fit <- stan("worldcup.stan", data=data, chains=4, iter=2000)
print(fit)

colVars <- function(a) {n <- dim(a)[[1]]; c <- dim(a)[[2]]; return(.colMeans(((a - matrix(.colMeans(a, n, c), nrow = n, ncol = c, byrow = TRUE)) ^ 2), n, c) * n / (n - 1))}

sims <- extract(fit)
a_sims <- sims$a
a_hat <- colMeans(a_sims)
a_se <- sqrt(colVars(a_sims))
library ("arm")
png ("worldcup1.png", height=500, width=500)
coefplot (rev(a_hat), rev(a_se), CI=1, varnames=rev(teams), main="Team quality (estimate +/- 1 s.e.)\n", cex.var=.9, mar=c(0,4,5.1,2), xlim=c(-2,2))
dev.off()


expected_on_sqrt_scale <- a_hat[team1] - a_hat[team2]
sigma_y_sims <- sims$sigma_y
interval_975 <- median(qt(.975,df)*sigma_y_sims)
signed_square <- function (a) {sign(a)*a^2}
lower <- signed_square(expected_on_sqrt_scale - interval_975)
upper <- signed_square(expected_on_sqrt_scale + interval_975)

coefplot (rev(score1 - score2), sds=rep(0, ngames),
          lower.conf.bounds=rev(lower), upper.conf.bounds=rev(upper), 
          varnames=rev(paste(teams[team1], "vs.", teams[team2])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))



prior_score <- rev(1:nteams) %>% scale() %>% {.*.5}
team1 <- match(as.vector(season17_sparse$Team1),teams)              
team2 <- match(as.vector(season17_sparse$Team2),teams)           
score1 <- as.vector(season17_sparse$Goals1)
score2 <- as.vector(season17_sparse$Goals2)
sqrt_diff <- score1-score2

df <- 7

library(rstan)



s_data <- list(team1 = team1,
               team2 = team2,
               nteams = as.integer(nteams),
               prior_score =prior_score %>% as.numeric(),
               sqrt_diff = sqrt_diff,
               df = df)

fit <- stan(file = "model.stan",
            data = s_data)

