# Functions ------------------------------------------------------------------

footballr <- function(start,end){
  seasons <- start$season:end$season
  matchdays <- list()
  for(i in seq_along(seasons)){
    matchdays[[i]] <- 1:34
  }
  names(matchdays) <- paste0("season",seasons)
  matchdays[[1]] <- start$matchday:34
  matchdays[[length(matchdays)]] <- 1:end$matchday
  
  dwld <- list()
  #i <- 1
  #ii <- 1
  for(i in seq_along(seasons)){
    dwld[[i]] <- list()
    for(ii in matchdays[[i]]){
      path <- sprintf("https://www.openligadb.de/api/getmatchdata/bl1/20%2.0f/%.0f",
                      seasons[i],ii)
      dwld[[i]][[ii]] <- read_json(path)
    }
  }
  #dwld[[1]][[2]]
  names(dwld) <- paste0("season",seasons)
  dwld
}


extract_info <- function(x){
  laply(x,function(x){ 
    c(
      "MatchDateTime" = x[["MatchDateTime"]],
      "MatchID" = x[["MatchID"]],
      "LeagueName"= x[["LeagueName"]],
      "MatchDay" = x[["Group"]][["GroupName"]],
      "Team1" = x[["Team1"]][["TeamName"]],
      "Team2" = x[["Team2"]][["TeamName"]],
      "Goals1" = x[["MatchResults"]][[2]][["PointsTeam1"]],
      "Goals2" = x[["MatchResults"]][[2]][["PointsTeam2"]]
    )
  }) %>% as_tibble()
}


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

extractor <- function(X) purrr::map(X,extract_info) %>% bind_rows()

compute_table <- function(season){
  season <- within(season,{
    Goals1 %<>% as.numeric()
    Goals2 %<>% as.numeric()
    Points2 <- (Goals1 < Goals2)*3 + (Goals1 == Goals2)*1
    Points1 <- (Goals1 > Goals2)*3 + (Goals1 == Goals2)*1
    MatchDay %<>% str_extract("\\d{1,2}") %>% as.numeric()
    diff1 <- Goals1-Goals2
    diff2 <- Goals2-Goals1
  })
  
  long <- cbind(
    season %>% select(Team1,Team2) %>% gather(),
    season %>% select(Points1,Points2) %>% gather()
  )[,c(2,4)]
  
  names(long) <- c("Team","Points")
  long %>% group_by(Team) %>% summarise(Points = sum(Points)) %>%
    arrange(desc(Points))
}


# Model preparation 

model_preparation <- function(team_ranking,seasons,whichseasons,maxmatchday){
  
  # Select only relevant games (in whichseason-vector AND [lower than maxmatchday OR not lastseason])
  seasons <- seasons %>% filter(season %in% whichseasons,
                                (MatchDay <= maxmatchday | season != whichseasons[length(whichseasons)]))
  
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
  
  datalist <- list(nteams,ngames,team1,score1,team2,score2,prior_score)
  names(datalist) <- c("nteams","ngames","team1","score1","team2","score2","prior_score")
  
  return(datalist)
}


# Prediction functions

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
#////////////////////////////////////////////////////////////////////
sample_game_c <- function(b) sample_game(b,team_n[1],team_n[2])

simulate_matchday <- function(empty,samps,team_ranking){
  
  res <- list()
  for(i in 1:nrow(empty)){
    
    # Assigns team id-number of game in row i to team_n vector
    team_n <- empty[i,c("Team1","Team2")] %>% match(team_ranking)
    
    samples <- apply(samps,1,sample_game_c) %>% t()
    
    probs <- samples %>% 
      as_tibble() %>%
      group_by(goals1,goals2) %>%
      summarise("n" = n()/nrow(samps)) %>%
      arrange(desc(n)) 
      
    
    res[[i]] <- 
      data.frame(team1 = team_n[1],
                 team2 = team_n[2],
                 MatchID = empty[i,c("MatchID")],
                 probs)
  }
  names(res) <- paste0(empty$Team1," - ",empty$Team2) 
  
  return(res)
}


#########################

simulate_matchday_payoffs <- function(empty,samps){
  
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
  
  return(res2)
}

#End Functions------------------------------------------------------------------
