library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(zoo)

thedata_paths <- fs::dir_ls("data/clean_data")

df <- purrr::map_dfr(thedata_paths,~{
  readr::read_csv(.x)
})

# ── 1. ESTIMATE POSSESSIONS & DERIVED STATS ───────────────────────────────────
df <- df %>%
  mutate(
    TeamPoss = 0.5 * (
      (TeamFGA - TeamORB + TeamTOV + 0.44 * TeamFTA) +
        (OpponentFGA - OpponentORB + OpponentTOV + 0.44 * OpponentFTA)
    ),
    HomeOffRtg   = (ScoreTm / TeamPoss) * 100,
    HomeDefRtg   = (ScoreOpp / TeamPoss) * 100,
    AwayOffRtg   = (ScoreOpp / TeamPoss) * 100,
    AwayDefRtg   = (ScoreTm / TeamPoss) * 100,
    TotalPoints  = ScoreTm + ScoreOpp
  )

# ── 2. PIVOT TO LONG FORMAT (one row per team per game) ───────────────────────
# This lets us compute each team's rolling stats regardless of home/away role

home_long <- df %>%
  select(gameid, Gtm, season, team_full, Opp, TotalPoints, TeamPoss,
         pts_scored = ScoreTm, pts_allowed = ScoreOpp,
         off_rtg = HomeOffRtg, def_rtg = HomeDefRtg,
         fga = TeamFGA, fta = TeamFTA, three_pa = Team3PA,
         efg_pct = TeameFG_pct, opp_efg_pct = OpponenteFG_pct,
         tov = TeamTOV, orb = TeamORB,
         opp_tov = OpponentTOV, opp_orb = OpponentORB) %>%
  mutate(home_away = "home", team = team_full)

away_long <- df %>%
  select(gameid, Gtm, season, team_full, Opp, TotalPoints, TeamPoss,
         pts_scored = ScoreOpp, pts_allowed = ScoreTm,
         off_rtg = AwayOffRtg, def_rtg = AwayDefRtg,
         fga = OpponentFGA, fta = OpponentFTA, three_pa = Opponent3PA,
         efg_pct = OpponenteFG_pct, opp_efg_pct = TeameFG_pct,
         tov = OpponentTOV, orb = OpponentORB,
         opp_tov = TeamTOV, opp_orb = TeamORB) %>%
  mutate(home_away = "away", team = Opp)

long_df <- bind_rows(home_long, away_long) %>%
  arrange(team, season, Gtm)

# ── 3. ROLLING HELPERS ────────────────────────────────────────────────────────
roll_lag <- function(x, n) {
  rollmean(lag(x, 1), k = n, fill = NA, align = "right")
}

# ── 4. COMPUTE ROLLING FEATURES PER TEAM ──────────────────────────────────────
team_rolling <- long_df %>%
  group_by(team) %>%
  mutate(
    # Scoring
    roll5_pts_scored    = roll_lag(pts_scored, 5),
    roll10_pts_scored   = roll_lag(pts_scored, 10),
    roll5_pts_allowed   = roll_lag(pts_allowed, 5),
    roll10_pts_allowed  = roll_lag(pts_allowed, 10),

    # Pace & Ratings
    roll5_poss          = roll_lag(TeamPoss, 5),
    roll10_poss         = roll_lag(TeamPoss, 10),
    roll5_off_rtg       = roll_lag(off_rtg, 5),
    roll10_off_rtg      = roll_lag(off_rtg, 10),
    roll5_def_rtg       = roll_lag(def_rtg, 5),
    roll10_def_rtg      = roll_lag(def_rtg, 10),

    # Shooting & Efficiency
    roll5_fga           = roll_lag(fga, 5),
    roll5_fta           = roll_lag(fta, 5),
    roll5_3pa           = roll_lag(three_pa, 5),
    roll5_efg_pct       = roll_lag(efg_pct, 5),
    roll5_opp_efg_pct   = roll_lag(opp_efg_pct, 5),

    # Possession Drivers
    roll5_tov           = roll_lag(tov, 5),
    roll5_orb           = roll_lag(orb, 5),
    roll5_opp_tov       = roll_lag(opp_tov, 5),
    roll5_opp_orb       = roll_lag(opp_orb, 5)
  ) %>%
  ungroup()



# ── 4.5. EWMA FEATURES (more weight on recent games) ────────────────────────────

ewma_lag <- function(x, alpha = 0.3) {
  n <- length(x)
  result <- numeric(n)
  result[1] <- NA

  for (i in 2:n) {
    vals <- x[1:(i-1)]
    weights <- (1 - alpha)^(rev(seq_along(vals)) - 1)
    non_na <- !is.na(vals)
    if (any(non_na)) {
      result[i] <- sum(vals[non_na] * weights[non_na]) / sum(weights[non_na])
    } else {
      result[i] <- NA
    }
  }
  return(result)
}

team_rolling <- team_rolling %>%
  group_by(team_full) %>%
  mutate(
    ewma_pts_scored  = ewma_lag(pts_scored,   alpha = 0.3),
    ewma_pts_allowed = ewma_lag(pts_allowed,  alpha = 0.3),
    ewma_poss        = ewma_lag(TeamPoss,  alpha = 0.3),
    ewma_off_rtg     = ewma_lag(off_rtg,    alpha = 0.3),
    ewma_def_rtg     = ewma_lag(def_rtg,    alpha = 0.3)
  ) %>%
  ungroup()


# ── 5. PIVOT BACK TO GAME LEVEL ───────────────────────────────────────────────
# Split home and away, then rejoin on gameid

rolling_cols <- c("roll5_pts_scored", "roll10_pts_scored",
                  "roll5_pts_allowed", "roll10_pts_allowed",
                  "roll5_poss", "roll10_poss",
                  "roll5_off_rtg", "roll10_off_rtg",
                  "roll5_def_rtg", "roll10_def_rtg",
                  "roll5_fga", "roll5_fta", "roll5_3pa",
                  "roll5_efg_pct", "roll5_opp_efg_pct",
                  "roll5_tov", "roll5_orb",
                  "roll5_opp_tov", "roll5_opp_orb",
                  "ewma_pts_scored", "ewma_pts_allowed",
                  "ewma_poss", "ewma_off_rtg", "ewma_def_rtg")

home_features <- team_rolling %>%
  filter(home_away == "home") %>%
  select(gameid, all_of(rolling_cols)) %>%
  rename_with(~ paste0("home_", .), all_of(rolling_cols))

away_features <- team_rolling %>%
  filter(home_away == "away") %>%
  select(gameid, all_of(rolling_cols)) %>%
  rename_with(~ paste0("away_", .), all_of(rolling_cols))

game_df <- df %>%
  left_join(home_features, by = "gameid") %>%
  left_join(away_features, by = "gameid")

# ── 6. DROP ROWS WITHOUT ENOUGH HISTORY ───────────────────────────────────────
game_df_clean <- game_df %>%
  filter(!is.na(home_roll10_pts_scored) & !is.na(away_roll10_pts_scored))

glimpse(game_df_clean)

unique(df$season)

game_df_clean <- game_df_clean %>%
  mutate(
    # Pace matchup — will this be a fast or slow game?
    pace_diff         = home_roll10_poss - away_roll10_poss,
    avg_combined_pace = (home_roll10_poss + away_roll10_poss) / 2,

    # Offense vs Defense matchup
    home_off_vs_away_def = home_roll10_off_rtg - away_roll10_def_rtg,
    away_off_vs_home_def = away_roll10_off_rtg - home_roll10_def_rtg,

    # Combined projected scoring
    implied_total = (home_roll10_pts_scored + away_roll10_pts_allowed) / 2 +
      (away_roll10_pts_scored + home_roll10_pts_allowed) / 2,

    # Efficiency gap
    efg_matchup_home = home_roll5_efg_pct - away_roll5_opp_efg_pct,
    efg_matchup_away = away_roll5_efg_pct - home_roll5_opp_efg_pct,

    # TOV matchup
    tov_matchup = home_roll5_tov + away_roll5_tov
  )

# modeling ----
library(tidyverse)
library(randomForest)
library(Metrics)  # for MAE, RMSE
library(caret)
# ── 1. DEFINE FEATURES & TARGET ───────────────────────────────────────────────
features <- c(
  # Home team rolling features
  "home_roll5_pts_scored", "home_roll10_pts_scored",
  "home_roll5_pts_allowed", "home_roll10_pts_allowed",
  "home_roll5_poss", "home_roll10_poss",
  "home_roll5_off_rtg", "home_roll10_off_rtg",
  "home_roll5_def_rtg", "home_roll10_def_rtg",
  "home_roll5_efg_pct", "home_roll5_opp_efg_pct",
  "home_roll5_tov", "home_roll5_orb",
  "home_ewma_pts_scored", "home_ewma_pts_allowed",
  "home_ewma_poss", "home_ewma_off_rtg", "home_ewma_def_rtg",

  # Away team rolling features
  "away_roll5_pts_scored", "away_roll10_pts_scored",
  "away_roll5_pts_allowed", "away_roll10_pts_allowed",
  "away_roll5_poss", "away_roll10_poss",
  "away_roll5_off_rtg", "away_roll10_off_rtg",
  "away_roll5_def_rtg", "away_roll10_def_rtg",
  "away_roll5_efg_pct", "away_roll5_opp_efg_pct",
  "away_roll5_tov", "away_roll5_orb",
  "away_ewma_pts_scored", "away_ewma_pts_allowed",
  "away_ewma_poss", "away_ewma_off_rtg", "away_ewma_def_rtg",

  # Interaction features
  'pace_diff',
  'avg_combined_pace',
  'home_off_vs_away_def',
  'away_off_vs_home_def',
  'implied_total',
  'efg_matchup_home',
  'efg_matchup_away',
  'tov_matchup'
)

target <- "TotalPoints"

# ── 2. DEFINE TRAIN / TEST SPLITS ─────────────────────────────────────────────
# Strategy: train on all years up to test year (expanding window)
# 2022-2024 → predict 2025
# 2022-2025 → predict 2026

train_2025 <- game_df_clean %>% filter(season %in% c(2022, 2023, 2024))
test_2025  <- game_df_clean %>% filter(season == 2025)

train_2026 <- game_df_clean %>% filter(season %in% c(2022, 2023, 2024, 2025))
test_2026  <- game_df_clean %>% filter(season == 2026)

# ── 3. HELPER TO PREP MATRIX ──────────────────────────────────────────────────
prep <- function(data) {
  data %>%
    select(all_of(c(features, target))) %>%
    drop_na()
}

train_2025_clean <- prep(train_2025)
test_2025_clean  <- prep(test_2025)
train_2026_clean <- prep(train_2026)
test_2026_clean  <- prep(test_2026)

# ── 4. TRAIN RANDOM FOREST ────────────────────────────────────────────────────
set.seed(42)

tune_grid <- expand.grid(mtry = c(3, 5, 7, 10, 15))

ctrl <- trainControl(method = "timeslice",
                     initialWindow = nrow(train_2025_clean) * 0.8,
                     horizon       = 50,
                     fixedWindow   = FALSE)

rf_tuned <- train(
  x         = train_2025_clean %>% select(all_of(features)),
  y         = train_2025_clean %>% pull(target),
  method    = "rf",
  trControl = ctrl,
  tuneGrid  = tune_grid,
  ntree     = 500,
  nodesize  = 5
)
print(rf_tuned$bestTune)

rf_2025 <- randomForest(
  x        = train_2025_clean %>% select(all_of(features)),
  y        = train_2025_clean %>% pull(target),
  ntree    = 500,
  mtry     = floor(sqrt(length(features))),  # default for regression
  nodesize = 5,
  importance = TRUE
)

rf_2026 <- randomForest(
  x        = train_2026_clean %>% select(all_of(features)),
  y        = train_2026_clean %>% pull(target),
  ntree    = 500,
  mtry     = floor(sqrt(length(features))), #default 6
  nodesize = 5,
  importance = TRUE
)

# ── 5. EVALUATE ───────────────────────────────────────────────────────────────
eval_model <- function(model, test_data, season_label) {
  preds  <- predict(model, newdata = test_data %>% select(all_of(features)))
  actual <- test_data %>% pull(target)

  tibble(
    Season    = season_label,
    MAE       = mae(actual, preds),
    RMSE      = rmse(actual, preds),
    Within_3  = mean(abs(actual - preds) <= 3),
    Within_5  = mean(abs(actual - preds) <= 5),
    Within_7  = mean(abs(actual - preds) <= 7)
  )
}

results <- bind_rows(
  eval_model(rf_2025, test_2025_clean, 2025),
  eval_model(rf_2026, test_2026_clean, 2026)
)

print(results)

# ── 6. VARIABLE IMPORTANCE ────────────────────────────────────────────────────
importance_df <- importance(rf_2025) %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  arrange(desc(`%IncMSE`))

importance_df %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Most Important Features (2025 model)",
       x = NULL, y = "% Increase in MSE")
