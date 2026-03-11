# cleaning data pipeline

library(tidyverse)
library(dplyr)
library(readr)
year = 2022

bb_season <- read_csv(paste0("data/raw_data/bb_",year,"_season.csv"))
masterlist <- read.csv("data/bb_Team_masterlist.csv")


glimpse(bb_season)


outlier_crosswalk <- setNames(
  c("Grambling State",
    "LIU",
    "Loyola Chicago",
    "UMBC",
    "UMass Lowell",
    "Ole Miss",
    "UNLV",
    "Prairie View A&M",
    "St. Mary's (CA)",
    "Sam Houston State",
    "USC Upstate",
    "SIU Edwardsville",
    "SMU",
    "UT Martin",
    "VCU",
    "VMI"),
  c("Grambling",
    "Long Island University",
    "Loyola (IL)",
    "Maryland Baltimore County",
    "Massachusetts Lowell",
    "Mississippi",
    "Nevada Las Vegas",
    "Prairie View",
    "Saint Mary's (CA)",
    "Sam Houston",
    "South Carolina Upstate",
    "Southern Illinois Edwardsville",
    "Southern Methodist",
    "Tennessee Martin",
    "Virginia Commonwealth",
    "Virginia Military Institute"))

bb_season <- bb_season %>%
  mutate(homeaway = case_when(...4 == '@' ~ 'H',
                              ...4 == 'N' ~ 'N',
                              is.na(...4) ~ 'A')) %>%
  filter(!is.na(Opp)) %>%
  filter(!is.na(ScoreRslt)) %>%
  left_join(masterlist %>% rename(team_full = team,
                                  team = team_url)) %>%
  mutate(Opp = stringr::str_replace_all(Opp,"–|-",' '),
         team_full = stringr::str_replace_all(team_full,"–|-",' '),
         team_full = coalesce(outlier_crosswalk[team_full],team_full))

sort(c("Ole Miss","Southern Mississippi"))[1] == "Ole Miss"



bb_season_uid <- bb_season %>%
  mutate(alphteam = pmin(team_full,Opp),
         gameid = case_when(homeaway == "H" ~ paste(Date,team_full,ScoreTm,Opp,ScoreOpp, sep = "_"),
                            homeaway == "A" ~ paste(Date,Opp,ScoreOpp,team_full,ScoreTm, sep = "_"),
                            homeaway == "N" & team_full == alphteam ~ paste(Date,team_full,ScoreTm,Opp,ScoreOpp, sep = "_"),
                            homeaway == "N" & Opp == alphteam ~ paste(Date,Opp,ScoreOpp,team_full,ScoreTm, sep = "_")
         )
         # gameid2 = paste(Date, min(team_full, Opp), max(team_full, Opp), pmin(ScoreTm, ScoreOpp), pmax(ScoreTm, ScoreOpp), sep = "_")
  )

geq3 <- bb_season_uid %>% group_by(gameid) %>% summarise(n = n()) %>% filter(n >= 3)

bb_season_uid %>%
  count(gameid) %>%
  count(n)

# bb_season_uid %>%
#   group_by(gameid) %>%
#   arrange((desc(homeaway))) %>%
#   bb_season_uid %>% summarise(n = n(),
#                                    ndist = n_distinct(gameid))

onegameonerow <- bb_season_uid %>%
  filter(homeaway %in% c("H","N") & !(gameid %in% geq3$gameid)) %>%
  distinct(gameid, .keep_all = TRUE) %>%
  mutate(Neutral = ifelse(homeaway == 'N',1,0))

write_csv(onegameonerow,paste0("data/clean_data/",year,"_season.csv"))
