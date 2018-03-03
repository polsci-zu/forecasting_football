
#-----------------------------------------------------------------
badr::remove_libraries()
badr::require_quite(c(
  "plyr","tidyverse","rvest","magrittr","stringr","jsonlite","rstan")
)

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

season17 <- llply(1:34,
                  function(i) read_json(
                    paste0("https://www.openligadb.de/api/getmatchdata/bl1/2016/",
                           i)))
season17 %<>% llply(extract_info) %>%
  bind_rows()

season18 <- llply(1:5,
                  function(i) read_json(
                    paste0("https://www.openligadb.de/api/getmatchdata/bl1/2017/",
                           i)))
season18
season18 %<>% llply(extract_info) %>%
  bind_rows()

seasons <- bind_rows(season17,season18,.id = "season")

# Gleitende Tabelle berechnen

seasons %>% arrange()group_by(


table(seasons$season)

seasons%<>% within({
  Goals1 %<>% as.numeric()
  Goals2 %<>% as.numeric()
  Points2 <- (Goals1 < Goals2)*3 + (Goals1 == Goals2)*1
  Points1 <- (Goals1 > Goals2)*3 + (Goals1 == Goals2)*1
  MatchDay %<>% str_extract("\\d{1,2}") %>% as.numeric()
  diff1 <- Goals1-Goals2
  diff2 <- Goals2-Goals1
  }
  ) 

season_l <- bind_rows(select(seasons,MatchID,Team = Team1,Diff = diff1),
          select(seasons,MatchID,Team = Team2,Diff = diff2),.id = "home")

season_l %<>% left_join(select(seasons,MatchID,MatchDay))

season_l %<>% arrange(Team,MatchDay) %>% group_by(Team)

library(TTR)
season_l %<>% mutate("ema3" = EMA(Diff,3),
                     "ema2" = EMA(Diff,2))
season_l %<>%  within({
    ema <- ema3
    ema[is.na(ema)] <- ema2[is.na(ema)]
    ema[is.na(ema)] <- Diff[is.na(ema)]
    })

season_l %<>% mutate(EMA = lag(ema))

season_l %<>%  within({
  EMA[is.na(EMA)] <- 0
})

season_l %<>% ungroup()


season_w <- select(season_l,home,EMA,MatchID) %>% spread(home,EMA)
names(season_w)[2:3] <- c("st1","st2")

seasons <- left_join(seasons,season_w)
#-----------------------------------------------------------------------




team_ranking <- c("Bayern München",
           "Borussia Dortmund",
           "Bayer 04 Leverkusen",
           "Borussia Mönchengladbach",
           "FC Schalke 04",
           "1. FSV Mainz 05",
           "Hertha BSC",
           "VfL Wolfsburg",
           "1. FC Köln", 
           "Hamburger SV",
           "FC Ingolstadt 04", 
           "FC Augsburg",
           "Werder Bremen",
           "SV Darmstadt 98",
           "TSG 1899 Hoffenheim", 
           "Eintracht Frankfurt", 
           "SC Freiburg",
           "RB Leipzig",
           "VfB Stuttgart",
           "Hannover 96")

save(team_ranking,seasons,file = "data.Rdata")
