library(rvest)
library(xml2)
library(data.table)
library(lubridate)
library(stringr)
library(tibble)
library(purrr)
library(curl)


teams_url <- "https://www.sports-reference.com/cbb/schools/"

page <- read_html(teams_url)

team_url_full <- html_nodes(page, '#NCAAM_schools tbody tr td[data-stat="school_name"] a') %>%
  html_attr("href")

the_teams <- html_nodes(page,"#NCAAM_schools tbody tr td[data-stat='school_name'] a") %>%
  html_text(trim = TRUE)

the_years <- html_nodes(page,"#NCAAM_schools tbody tr td[data-stat='year_max']") %>%
  html_text(trim = TRUE) %>%
  as.integer()

team_df <- tibble('team' = the_teams,
                  'year' = the_years,
                  'team_url' = team_url_full) %>%
  dplyr::filter(year >= 2016) %>%
  dplyr::mutate(team_url = stringr::str_remove_all(team_url,"/cbb/schools/|/men/"))

readr::write_csv(team_df,"data/bb_Team_masterlist.csv")



scrape_team_gamelog <- function(team, season) {

  url <- paste0(
    "https://www.sports-reference.com/cbb/schools/",
    team, "/men/", season, "-gamelogs.html"
  )

  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  # on.exit(close(page))
  table <- page %>%
    html_node("#team_game_log") %>%
    html_table(fill = TRUE) #%>%


  colnames(table) <- paste0(colnames(table), as.character(unlist(table[1,]))) %>%
    stringr::str_replace_all('%','_pct')
  table <- table[-1, ]
  rm(page)
  if (is.null(table)) return(NULL)

  dt <- as.data.table(table)

  dt[, team := team]
  dt[, season := season]

  return(dt)
}


seasons <- 2026

all_games_20 <- purrr::map(seasons, function(season) {

  purrr::map_dfr(team_df$team_url, function(team_url) {

    cli::cli_alert_info("Team: {team_url} Season: {season}")
    gc()
    Sys.sleep(runif(1, 3, 5))

    scrape_team_gamelog(team_url, season)
  })
})

all_games_20[[1]] %>% glimpse()

all_games_20[[1]] |> readr::write_csv("data/raw_data/bb_2026_season_pulled_mar7.csv")
# season <- 2025
# team_df$team[11:365]
# team_df$team[28]
# team_df$team[58]
# # bowling-green -
# # bradley-colorado-state
# all_games_2025 <- purrr::map(team_df$team_url[68:368],
#    ~{
#      gc()
#      Sys.sleep(runif(1, 1, 5))
#      scrape_team_gamelog(.x, season)
#      }
# )
#
# ag1_67 <- all_games_2025
# all_games_2025_1_10 <- all_games_2025
# all_games_2025_11_17_19_48 <- all_games_2025
# all_games_2025_3 <- all_games_2025
#
# all_games_2025

# temp1 <- all_games_2025_1_10 |> reduce(dplyr::bind_rows)
# temp2 <- all_games_2025_11_17_19_48 |> reduce(dplyr::bind_rows)
# temp3 <- all_games_2025_3|> reduce(dplyr::bind_rows)
# temp4 <- dplyr::bind_rows(temp1,temp2) |> dplyr::bind_rows(temp3)
#
# remaining_teams_to_pull <- setdiff(team_df$team_url,unique(temp4$team))
#
# all_games_2025_2 <- purrr::map(remaining_teams_to_pull,
#                              ~{
#                                gc()
#                                Sys.sleep(runif(1, 3, 5))
#                                scrape_team_gamelog(.x, season)
#                              }
# )
#
# temp5 <- temp4 |> dplyr::bind_rows(all_games_2025_2 |> reduce(dplyr::bind_rows))
#
# write_csv(temp5,"~/AnalyticsShare/David/projects/educational/2026_bbb/data/2025/bb_2025_season.csv")
#
# all_games_2024 <- purrr::map(team_df$team_url,
#                                ~{
#                                  gc()
#                                  Sys.sleep(runif(1, 3, 5))
#                                  scrape_team_gamelog(.x, 2024)
#                                }
# )
# all_games_2024 <- reduce(all_games_2024, dplyr::bind_rows)
#
# all_games_2023 <- purrr::map(team_df$team_url,
#                              ~{
#                                gc()
#                                Sys.sleep(runif(1, 3, 5))
#                                scrape_team_gamelog(.x, 2023)
#                              }
# ) |> reduce(dplyr::bind_rows)
#
# all_games_2022 <- purrr::map(team_df$team_url,
#                              ~{
#                                gc()
#                                Sys.sleep(runif(1, 3, 5))
#                                scrape_team_gamelog(.x, 2022)
#                              }
# ) |> reduce(dplyr::bind_rows)
#
# all_games_2026 <- purrr::map(team_df$team_url,
#                              ~{
#                                gc()
#                                Sys.sleep(runif(1, 3, 5))
#                                scrape_team_gamelog(.x, 2026)
#                              }
# ) |> reduce(dplyr::bind_rows)
#
#
# write_csv(all_games_2022,"~/AnalyticsShare/David/projects/educational/2026_bbb/data/byyear/bb_2022_season.csv")
# write_csv(all_games_2023,"~/AnalyticsShare/David/projects/educational/2026_bbb/data/byyear/bb_2023_season.csv")
# write_csv(all_games_2024,"~/AnalyticsShare/David/projects/educational/2026_bbb/data/byyear/bb_2024_season.csv")
#
# write_csv(temp5,"~/AnalyticsShare/David/projects/educational/2026_bbb/data/byyear/bb_2025_season.csv")

showConnections(all = TRUE)
closeAllConnections()

test <- scrape_team_gamelog(team_df$team[3],'2024')


# test ----
url <- paste0(
  "https://www.sports-reference.com/cbb/schools/",
  team_df$team[50], "/men/", "2024", "-gamelogs.html"
)

pagetemp <- tryCatch(read_html(url), error = function(e) NULL)
if (is.null(page)) return(NULL)

table <- page %>%
  html_node("#team_game_log") %>%
  html_table(fill = TRUE) #%>%


colnames(table) <- paste0(colnames(table), as.character(unlist(table[1,]))) %>%
  stringr::str_replace_all('%','_pct')
table <- table[-1, ]

if (is.null(table)) return(NULL)

dt <- as.data.table(table)

dt[, team := team]
dt[, season := season]
