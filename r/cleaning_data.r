library(tidyverse)
library(dplyr)
library(readr)
bb_2026_season <- read_csv("data/raw_data/bb_2026_season_pulled_mar7.csv")
masterlist <- read.csv("data/bb_Team_masterlist.csv")


glimpse(bb_2026_season)


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

bb_2026_season <- bb_2026_season %>%
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



bb_2026_season_uid <- bb_2026_season %>%
  mutate(alphteam = pmin(team_full,Opp),
    gameid = case_when(homeaway == "H" ~ paste(Date,team_full,ScoreTm,Opp,ScoreOpp, sep = "_"),
                            homeaway == "A" ~ paste(Date,Opp,ScoreOpp,team_full,ScoreTm, sep = "_"),
                            homeaway == "N" & team_full == alphteam ~ paste(Date,team_full,ScoreTm,Opp,ScoreOpp, sep = "_"),
                            homeaway == "N" & Opp == alphteam ~ paste(Date,Opp,ScoreOpp,team_full,ScoreTm, sep = "_")
                            )
    # gameid2 = paste(Date, min(team_full, Opp), max(team_full, Opp), pmin(ScoreTm, ScoreOpp), pmax(ScoreTm, ScoreOpp), sep = "_")
    )

bb_2026_season_uid %>%
  count(gameid) %>%
  count(n)

bb_2026_season_uid %>%
  group_by(gameid) %>%
  arrange((desc(homeaway))) %>%

bb_2026_season_uid %>% summarise(n = n(),
                                 ndist = n_distinct(gameid))
onegameonerow <- bb_2026_season_uid %>%
  filter(homeaway %in% c("H","N")) %>%
  distinct(gameid, .keep_all = TRUE) %>%
  mutate(Neutral = ifelse(homeaway == 'N',1,0))

write_csv(onegameonerow,"data/clean_data/2026_season_march7.csv")

# Scratch 2 ----
# bad_rows <- which(is.na(bb_2026_season_uid$gameid))
#
# checkdata <- bb_2026_season_uid %>% slice(bad_rows)
#
# checkdata %>% sample_n(20) %>% select(Opp,ScoreOpp,team_full,ScoreTm,Date,homeaway,gameid,alphteam)
#
# report_nas(bb_2026_season_uid)
#
# bb_2026_season_uid %>%
#   sample_n(20) %>%
#   select(Opp,ScoreOpp,team_full,ScoreTm,Date,homeaway,gameid) %>%
#   head(20)
#
# bb_2026_season_uid %>% select(Opp,ScoreOpp,team_full,ScoreTm,Date,homeaway,gameid) %>%
#   filter(is.na(gameid)) %>% head(10)
#
# bb_2026_season_uid %>% select(Opp,ScoreOpp,team_full,ScoreTm,Date,homeaway,gameid) %>%
#   filter(is.na(gameid)) %>% glimpse()



# Scratch ----
# tibble::tibble(team = c(sort(unique(bb_2026_season$team_full)),rep('',363)),
#                opp = sort(unique(bb_2026_season$Opp)))
#
# tibble::tibble(team = sort(unique(bb_2026_season$team_full)),
#                opp = sort(unique(bb_2026_season$Opp)))
#
# problem_teams <- setdiff(unique(bb_2026_season$team_full),intersect(bb_2026_season$team_full,bb_2026_season$Opp))
#
# fuzzymatchteam <- function(no_match_name,possible_teams){
#   cli::cli_alert_info("Matching: {no_match_name} ")
#   matchedteam <- possible_teams[which(data.table::`%like%`(possible_teams,no_match_name))]
#   # if(identical(matchedteam, character(0))){
#   #   start_string <- paste0(stringr::str_split(no_match_name,' ')[[1]][1],' ')
#   #   matchedteam <- unique(bb_2026_season$Opp)[which(startsWith(unique(bb_2026_season$Opp),start_string))]
#   # }
#   cli::cli_alert_info("Matched to: {matchedteam} ")
#   tibble(notmatch = no_match_name,matchname = matchedteam)
# }
#
# make_crosswalk <- function(no_match_vec,possible_teams){
#   length(no_match_vec)
#   purrr::map_dfr(no_match_vec, ~fuzzymatchteam(no_match_name = .x,possible_teams = possible_teams))
# }
#
#  opp_not_matched <- setdiff(unique(bb_2026_season$Opp),intersect(bb_2026_season$team_full,bb_2026_season$Opp))
#
# crosswalk <- make_crosswalk(problem_teams,opp_not_matched)
# print(crosswalk,n = Inf)
#
# setdiff(problem_teams,crosswalk$notmatch)
#
#
# sort(unique(bb_2026_season$Opp))
# paste0(stringr::str_split(problem_teams[2],' ')[[1]][1],' ',stringr::str_split(problem_teams[2],' ')[[1]][2])
#
# unique(bb_2026_season$Opp)[which(startsWith(unique(bb_2026_season$Opp),"Long "))]
#
# unique(bb_2026_season$Opp)[which(data.table::`%like%`(unique(bb_2026_season$Opp),"Long Island University"))]
