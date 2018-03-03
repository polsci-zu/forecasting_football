
badr::remove_libraries()
badr::require_quite(c(
  "plyr","tidyverse","rvest","magrittr","stringr","jsonlite","rstan")
)
#-----------------------------------------------------------------

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load("model/data.Rdata")
load("model/model_pois.Rdata")

source("model/00_function.R")

#---------------------------------------------------------------------


season17 <- llply(1:34,
                  function(i) read_json(
                    paste0("https://www.openligadb.de/api/getmatchdata/bl1/2017/",
                           i)))


empty <- extract_info_pre(season17[[21]])
samps <- as.data.frame(fit) %>% {.[(nrow(.)/2+1):nrow(.),]} # only lower half of df

samps %>% select(contains("skill")) %>% head(50) %>% summarise_all(funs(mean(.)))
team_ranking

res <- simulate_matchday(empty,samps);res



#------------------------------------------------------------------------
# Pay-Offs

res2 <- simulate_matchday_payoffs(empty,samps);res2

mat <- ldply(res2,function(x) x[1,])
mat$game <- names(res2)

tab <- bind_rows(res,.id = "game")

