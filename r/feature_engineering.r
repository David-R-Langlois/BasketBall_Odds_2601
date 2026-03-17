library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(zoo)

thedata_paths <- fs::dir_ls("data/clean_data")

df <- purrr::map_dfr(thedata_paths,~{
  readr::read_csv(.x)
})

glimpse(df)
df %>% arrange(desc(Date)) %>% glimpse()
df <- df %>%
  mutate(
    home_pace = (TeamFGA - TeamORB + TeamTOV + 0.44 * TeamFTA),
    away_pace = (OpponentFGA - OpponentORB + OpponentTOV + 0.44 * OpponentFTA),
    pace = 0.5 * (home_pace + away_pace),
    # Offensive Rating (points per 100 possessions)
    home_offrtg = (ScoreTm/pace) * 100,
    # Defensive Rating (points per 100 possessions)
    home_defrtg = (ScoreOpp/pace) * 100,
    TotalPoints = ScoreTm+ScoreOpp
    )
df <- df %>%
  arrange(team_full, Gtm)

# ── 3. ROLLING AVERAGE HELPER ─────────────────────────────────────────────────
# shift(1) ensures we NEVER include the current game (prevents data leakage)
roll_lag <- function(x, n) {
  lag_x <- lag(x, 1)
  rollmean(lag_x, k = n, fill = NA, align = "right")
}

# ── 4. BUILD ROLLING FEATURES ─────────────────────────────────────────────────
df <- df %>%
  group_by(team_full) %>%
  mutate(
    # --- Scoring ---
    roll5_pts_scored    = roll_lag(ScoreTm, 5),
    roll10_pts_scored   = roll_lag(ScoreTm, 10),
    roll5_pts_allowed   = roll_lag(ScoreOpp, 5),
    roll10_pts_allowed  = roll_lag(ScoreOpp, 10),
    roll5_total_pts     = roll_lag(TotalPoints, 5),
    roll10_total_pts    = roll_lag(TotalPoints, 10),

    # --- Pace & Ratings ---
    roll5_poss          = roll_lag(pace, 5),
    roll10_poss         = roll_lag(pace, 10),
    roll5_off_rtg       = roll_lag(home_offrtg, 5),
    roll10_off_rtg      = roll_lag(home_offrtg, 10),
    roll5_def_rtg       = roll_lag(home_defrtg, 5),
    roll10_def_rtg      = roll_lag(home_defrtg, 10),

    # --- Shooting Volume ---
    roll5_fga           = roll_lag(TeamFGA, 5),
    roll5_fta           = roll_lag(TeamFTA, 5),
    roll5_3pa           = roll_lag(Team3PA, 5),

    # --- Efficiency ---
    roll5_efg_pct       = roll_lag(TeameFG_pct, 5),
    roll10_efg_pct      = roll_lag(TeameFG_pct, 10),
    roll5_opp_efg_pct   = roll_lag(OpponenteFG_pct, 5),

    # --- Possession Drivers ---
    roll5_tov           = roll_lag(TeamTOV, 5),
    roll5_orb           = roll_lag(TeamORB, 5),
    roll5_opp_tov       = roll_lag(OpponentTOV, 5),
    roll5_opp_orb       = roll_lag(OpponentORB, 5),

    # --- Rest / Schedule ---
    days_rest           = Gtm - lag(Gtm, 1),  # proxy; replace with actual dates if you have them
    is_back_to_back     = as.integer(days_rest <= 1)
  ) %>%
  ungroup()

# ── 5. EWMA FEATURES (more weight on recent games) ────────────────────────────
ewma_lag <- function(x, alpha = 0.3) {
  n <- length(x)
  result <- rep(NA, n)
  for (i in 2:n) {
    vals <- x[1:(i-1)]  # lag: never include current game
    weights <- (1 - alpha)^(rev(seq_along(vals)) - 1)
    result[i] <- sum(vals * weights, na.rm = TRUE) / sum(weights[!is.na(vals)])
  }
  result
}

df <- df %>%
  group_by(team_full) %>%
  mutate(
    ewma_pts_scored  = ewma_lag(ScoreTm,   alpha = 0.3),
    ewma_pts_allowed = ewma_lag(ScoreOpp,  alpha = 0.3),
    ewma_poss        = ewma_lag(pace,  alpha = 0.3),
    ewma_off_rtg     = ewma_lag(home_offrtg,    alpha = 0.3),
    ewma_def_rtg     = ewma_lag(home_defrtg,    alpha = 0.3)
  ) %>%
  ungroup()

# ── 6. CREATE THE MATCHUP-LEVEL FEATURE MATRIX ────────────────────────────────
# Each game exists twice in your data (once per team).
# Join team features against opponent features for modeling.

team_features <- df %>%
  select(Team, Gtm, Opp, starts_with("roll"), starts_with("ewma"),
         is_back_to_back, TotalPoints, ScoreTm, ScoreOpp)

game_df <- team_features %>%
  inner_join(
    team_features %>%
      select(Team, Gtm, starts_with("roll"), starts_with("ewma")) %>%
      rename_with(~ paste0("opp_", .), starts_with("roll")) %>%
      rename_with(~ paste0("opp_", .), starts_with("ewma")),
    by = c("Opp" = "Team", "Gtm" = "Gtm")
  )

# ── 7. DROP EARLY ROWS WITH NA (not enough history yet) ───────────────────────
game_df_clean <- game_df %>%
  filter(!is.na(roll10_pts_scored))  # ensures 10+ games of history exist

# Quick check
glimpse(game_df_clean)


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
