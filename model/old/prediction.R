
badr::remove_libraries()
badr::require_quite(c(
  "plyr","tidyverse","rvest","magrittr","stringr","jsonlite","rstan")
)
#-----------------------------------------------------------------

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("data.Rdata")
load("model_pois.Rdata")

#---------------------------------------------------------------------

extract_info_pre <- function(x){
  laply(x,function(x){ 
    c(
      "MatchDateTime" = x[["MatchDateTime"]],
      "MatchID" = x[["MatchID"]],
      "LeagueName"= x[["LeagueName"]],
      "MatchDay" = x[["Group"]][["GroupName"]],
      "Team1" = x[["Team1"]][["TeamName"]],
      "Team2" = x[["Team2"]][["TeamName"]],
      "Goals1" = NA,
      "Goals2" = NA
    )
  }) %>% as_tibble()
}

season17 <- llply(1:34,
                  function(i) read_json(
                    paste0("https://www.openligadb.de/api/getmatchdata/bl1/2017/",
                           i)))


empty <- extract_info_pre(season17[[6]])
samps <- as.data.frame(fit) %>%
{.[(nrow(.)/2+1):nrow(.),]}

#////////////////////////////////////////////////////////////////////
sample_game <- function(b,h,a){
  home <- paste0("skill[",h,"]")
  away <- paste0("skill[",a,"]")
  g1 <- exp(b["baseline"] + b["home"] + b[home] - b[away])
  g2 <- exp(b["baseline"] + b[away] - b[home])
  game <- c(rpois(1,as.numeric(g1)),rpois(1,as.numeric(g2)))
  names(game) <- c("goals1","goals2")
  return(game)
}
sample_game_c <- function(b) sample_game(b,team_n[1],team_n[2])
#////////////////////////////////////////////////////////////////////

res <- list()
for(i in 1:nrow(empty)){
  team_n <- empty[i,c("Team1","Team2")] %>% match(team_ranking)
  
  samples <- apply(samps,1,sample_game_c) %>% t()
  
  probs <- samples %>% 
    as_tibble() %>%
    group_by(goals1,goals2) %>%
    summarise("n" = n()/nrow(samps)) %>%
    arrange(desc(n))
  res[[i]] <- probs
}
names(res) <- paste0(empty$Team1," - ",empty$Team2) 
res

#------------------------------------------------------------------------

res2 <- list()
for(i in 1:nrow(empty)){
  #i <- 1
  team_n <- empty[i,c("Team1","Team2")] %>% match(team_ranking)
  
  tipps <- expand.grid(0:10,0:10) %>% as_tibble()
  names(tipps) <- c("goals1","goals2")
  tipps %<>% within({
    diff <- goals1-goals2
    type <- sign(diff)
    })
  samples <- apply(samps,1,sample_game_c) %>% t() %>% as_tibble()
  samples %<>% within({
    diff <- goals1-goals2
    type <- sign(diff)
  })
  
  #ii <- 1
    
  for(ii in 1:nrow(tipps)){
    payoff <- rep(0,nrow(samples))
    payoff[samples$type == tipps$type[ii]] <- 2
    payoff[samples$diff == tipps$diff[ii]] <- 3
    payoff[samples$goals1 == tipps$goals1[ii] &
             samples$goals2 == tipps$goals2[ii]] <- 4
    tipps[ii,"payoff"] <- mean(payoff)
  }
  
  tipps <- arrange(tipps,desc(payoff))
  
  res2[[i]] <- tipps
}
names(res2) <- paste0(empty$Team1," - ",empty$Team2) 
res2

mat <- ldply(res2,function(x) x[1,])
mat$game <- names(res2)



