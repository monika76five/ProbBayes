get_onbase_data <- function(byear, TABB){
  library(Lahman)
  library(tidyverse)
  
  # pull out players born in the specific year
  
  Master %>% filter(birthYear %in% byear) %>%
    select(playerID) %>% pull() -> player_list
  
  # collect hitters with at least TABB at-bats
  
  Batting %>% filter(playerID %in% player_list) %>%
    group_by(playerID) %>%
    summarize(AB = sum(AB)) -> S1
  
  S1 %>%
    filter(AB >= TABB) %>%
    pull(playerID) -> player_list2000
  
  # collect stats for all players for all seasons
  
  Batting %>% filter(playerID %in% player_list2000) %>%
    group_by(playerID, yearID) %>%
    summarize(AB = sum(AB),
              BB = sum(BB),
              HBP = sum(HBP),
              SF = sum(SF),
              H = sum(H),
              OB = H + BB + HBP,
              PA = AB + BB + HBP + SF) -> d
  
  # define age variable, and age deviation from 30
  
  select(Master, playerID, birthYear, birthMonth) %>%
    inner_join(d, by=c("playerID")) %>%
    mutate(birthyear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear),
           Age = yearID - birthyear,
           AgeD = Age - 30) %>%
    select(playerID, Age, AgeD, PA, OB) -> d1
  
  # add names to data frame and create numerical id for
  # player using factors
  
  d1 %>% inner_join(select(Master, playerID, nameFirst,
                           nameLast),
                    by = "playerID") %>%
    mutate(Name = paste(nameFirst, nameLast)) %>%
    select(nameLast, Age, AgeD, PA, OB) -> d1
  
  d1$Player <- as.numeric(as.factor(d1$nameLast))
  d1
}