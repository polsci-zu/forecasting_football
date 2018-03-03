


badr::remove_libraries()
badr::require_quite(c(
  "plyr","tidyverse","rvest","magrittr","stringr","jsonlite")
)

### Hier gehts los ###-------------------------------------------------------

res_games <- list()
for(ii in 1:34){

  s <- html_session(paste0("https://www.fussballdaten.de/bundesliga/2017/",ii,"/"))


  # Fetch links to institute subsites ------------------------------------------
  page <- s %>%
    read_html()
  
  games <- page %>%
    html_nodes(xpath = "//*[@class = 'text vam']") %>%
    html_attr('href')
   
  for(i in games[1:9]){
  # i <- games[[1]] 
   game_i <- jump_to(s,i) 
   
   stats_i <- game_i %>% html_nodes(xpath = "//*[@class = 'statistik-reihe']")
   home_i <- stats_i %>% llply(html_text) %>%
     str_extract("^\\d{1,4}")
   
   away_i <- stats_i %>% llply(html_text) %>%
     str_extract("\\d{1,4}$")
   
   cat_i <- stats_i %>% llply(html_text) %>%
     str_replace_all("\\d","")
   
   tab_i <- tibble("category" = cat_i,"team1" = home_i,"team2" = away_i)
   tab_i$MatchDay <- ii
   res_games[[i]] <- tab_i
  }
}

add_info <- bind_rows(res_games,.id = "game_id")
add_info$team1 %<>% as.numeric()
add_info$team2 %<>% as.numeric()
add_info$diff <- add_info$team1-add_info$team2

sp <- add_info %>% select(diff,game_id,category) %>% spread(category,diff)
kuck <- sp[,-1] %>% cor() %>% round(2)

save(add_info,file = "additional_information_2017")

#- Now the current season.




