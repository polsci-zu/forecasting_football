
#-----------------------------------------------------------------
badr::remove_libraries()
badr::require_quite(c(
  "plyr","tidyverse","rvest","magrittr","stringr","jsonlite","rstan")
)

#start <- list("matchday" = 1,"season" = 17)
#end <- list("matchday" = 21,"season" = 18)

source("model/00_function.R")
#------------------------------------------------------------------

start <- list("matchday" = 1,"season" = 16)
end <- list("matchday" = 23,"season" = 17)

data_list <- footballr(start,end)

data_table <- data_list %>% purrr::map(extractor) %>%
  bind_rows(.id = "season")

# Gleitende Tabelle berechnen


table(data_table$season)

data_table%<>% within({
  Goals1 %<>% as.numeric()
  Goals2 %<>% as.numeric()
  Points2 <- (Goals1 < Goals2)*3 + (Goals1 == Goals2)*1
  Points1 <- (Goals1 > Goals2)*3 + (Goals1 == Goals2)*1
  MatchDay %<>% str_extract("\\d{1,2}") %>% as.numeric()
  diff1 <- Goals1-Goals2
  diff2 <- Goals2-Goals1
  }
  ) 
data_table$season
table(data_table$season)

s <- data_table[data_table$season == "season16",]

seasons <- data_table
# -----------------------------------------------------------------------

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

save(team_ranking,seasons,file = "model/data.Rdata")
