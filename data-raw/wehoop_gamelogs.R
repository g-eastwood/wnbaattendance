
library(tidyverse)
library(wehoop)

## Wrangling gamelog data from wehoop:

# 1. Pull league-wide gamelogs from wehoop

seasons <- 2016:2024

fetch_gamelog_season <- function(s) {
  res <- tryCatch(
    wnba_leaguegamelog(season = s, season_type = "Regular Season", league_id = "10"),
    error = function(e) {
      message(sprintf("wnba_leaguegamelog failed for season %s: %s", s, e$message))
      return(NULL)
    }
  )
  if (is.null(res)) return(NULL)
  
  as_tibble(res) |> 
    mutate(season = s)
}

gamelogs_raw <- seasons |>
  map(fetch_gamelog_season) |>
  bind_rows()

gamelogs <- gamelogs_raw$LeagueGameLog

# 2. clean gamelogs
gamelogs <- gamelogs |>
  mutate(
    game_date = as.Date(GAME_DATE),
    win_flag = if_else(WL == "W", 1L, 0L),
    isHome    = str_detect(MATCHUP, "vs"),
    isAway    = str_detect(MATCHUP, "@")
  ) |>
  arrange(TEAM_ID, game_date)

# 3. rolling win counts for 44 past games (length of a wnba season)
wnba_gamelogs <- gamelogs |>
  group_by(TEAM_ID) |>
  mutate(
    # rolling window of PREVIOUS 44 games, so exclude current game
    rolling_wins_44 =
      slider::slide_int(
        win_flag,
        ~ sum(.x, na.rm = TRUE),
        .before = 44,
        .after = -1,      # exclude current game
        .complete = FALSE
      ),
    
    rolling_games_44 =
      slider::slide_int(
        win_flag,
        ~ length(.x),
        .before = 44,
        .after = -1,
        .complete = FALSE
      )
  ) |>
  ungroup()


write_rds(wnba_gamelogs, file = "data/wnba_gamelogs.rds", compress = "xz")
