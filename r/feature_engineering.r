library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

thedata_paths <- fs::dir_ls("data/clean_data")

df <- purrr::map_dfr(thedata_paths,~{
  readr::read_csv(.x)
})

glimpse(df)

df <- df %>%
  mutate(
    home_poss = 0.5 * (
      (TeamFGA - TeamORB + TeamTOV + 0.44 * TeamFTA) +
        (OpponentFGA - OpponentORB + OpponentTOV + 0.44 * OpponentFTA)),
    # Offensive Rating (points per 100 possessions)
    home_offrtg = (ScoreTm/team_poss) * 100,
    home_defrtg = (ScoreOpp/team_poss) * 100,
    )

# TeamPoss = 0.5 * (
#   (TeamFGA - TeamORB + TeamTOV + 0.44 * TeamFTA) +
#     (OpponentFGA - OpponentORB + OpponentTOV + 0.44 * OpponentFTA)
# )

# Pace (possessions per 48 minutes — assumes full game, adjust if OT)
# Pace = TeamPoss  # possessions per game is fine for modeling purposes

# Offensive Rating (points per 100 possessions)
# OffRtg = (ScoreTm / TeamPoss) * 100

# Defensive Rating
# DefRtg = (ScoreOpp / TeamPoss) * 100
# df %>%
#   group_by(Date) %>%
#   summarise(team_full,Opp) %>% glimpse()

df %>% mutate(total_score = ScoreTm + ScoreOpp) %>%
  ggplot(aes(x = total_score)) +
  ggplot2::geom_histogram()

df %>% group_by()
rollingmean
