library(rvest)
library(xml2)
library(data.table)
library(lubridate)
library(stringr)
library(tibble)
library(purrr)

teams_url <- "https://www.sports-reference.com/cbb/schools/"
page <- read_html(teams_url)

# page %>% html_text() %>% substr(1,2000)
# page <- read_html(teams_url)

html_nodes(page, '#NCAAM_schools tbody tr td[data-stat="school_name"]') %>%
  html_text()

the_teams <- html_nodes(page,"#NCAAM_schools tbody tr td[data-stat='school_name'] a") %>%
  html_text(trim = TRUE)

the_years <- html_nodes(page,"#NCAAM_schools tbody tr td[data-stat='year_max']") %>%
  html_text(trim = TRUE) %>%
  as.integer()

team_df <- tibble('team' = the_teams,'year' = the_years) %>%
  dplyr::filter(year == '2026') %>%
  dplyr::mutate(
    team = trimws(stringr::str_remove_all(team,"[\\\\(\\\\)\\\\&\\\\.]")),
    team = tolower(stringr::str_replace_all(team,' ','-')))


scrape_team_gamelog <- function(team, season) {

  url <- paste0(
    "https://www.sports-reference.com/cbb/schools/",
    team, "/men/", season, "-gamelogs.html"
  )

  page <- tryCatch(read_html(url), error = function(e) NULL)
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

  return(dt)
}


season <- 2025

all_games_2024 <- purrr::map(team_df$team,
   ~{scrape_team_gamelog(.x, season)}
)

  rbindlist(
  lapply(the_teams, function(t) {
    Sys.sleep(1)
    scrape_team_gamelog(t, season)
  }),
  fill = TRUE
)











test <- scrape_team_gamelog('duke','2024')


# test ----
url <- paste0(
  "https://www.sports-reference.com/cbb/schools/",
  'duke', "/men/", "2022", "-gamelogs.html"
)

page <- tryCatch(read_html(url), error = function(e) NULL)
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
