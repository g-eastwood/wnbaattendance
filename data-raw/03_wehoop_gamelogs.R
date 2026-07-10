
library(tidyverse)
library(wehoop)

## Wrangling gamelog data from wehoop:

# 1. Pull league-wide gamelogs from wehoop

seasons <- 1997:2025

gamelogs_raw <- seasons |>
  map(~(wehoop::wnba_leaguegamelog(season = .x, season_type = "Regular Season", league_id = "10"))) |>
  bind_rows()

gamelogs <- gamelogs_raw$LeagueGameLog

# 2. clean gamelogs
gamelogs <- gamelogs |>
  janitor::clean_names() |>
  mutate(
    game_date = as.Date(game_date),
    season_id = parse_number(season_id) - 20000,
    win_flag = if_else(wl == "W", 1L, 0L),
    is_home    = str_detect(matchup, "vs"),
    is_away    = str_detect(matchup, "@"),
    opponent = str_extract(matchup, "[A-Z]{3}$")
  ) |>
  arrange(team_id, game_date)


write_rds(wnba_gamelogs, file = "data/wnba_gamelogs.rds", compress = "xz")
