library(tidyverse)

wnba_gl_elo <- read_rds(here::here("data/wnba_gl_elo.rds"))

# Add attendance to each game

wnba_attendance <- read_rds(here::here("data/wnba_attendance.rds"))

# Add metro area variables

cbsas <- read_rds(here::here("data/cbsas.rds"))

wnba_attendance <- wnba_attendance |>
  left_join(cbsas, by = join_by(season == year, cbsa_id == GEOID))

wnba_gl <- wnba_gl_elo |>
  left_join(
    wnba_attendance |>
      select(game_date, team, attendance, arena = arena_consolidated, arena_capacity, cbsa_id, median_income, total_pop), 
    by = join_by(game_date, team_name == team)
  )

wnba_gl |>
  filter(is.na(attendance)) |>
  group_by(season_id, team_id) |>
  count() |>
  print(n = Inf)

wnba_gl |>
  filter(is.na(attendance), team_id == 1611661325) |>
  group_by(team_name) |>
  count()

# San Antonio issue
wnba_gl |>
  filter(team_id == 1611661319) |>
  group_by(team_name, season_id) |>
  summarize(num_games = n(), num_missing = sum(is.na(attendance))) |>
  arrange(season_id) |>
  print(n = Inf)


# Add Caitlin Clark indicator

specific_players <- read_rds(here::here("data/wnba_gamelogs_cc.rds")) |>
  janitor::clean_names() |>
  mutate(
    cc = str_detect(player_name, "Caitlin") & min > 0,
    aw = str_detect(player_name, "A'ja") & min > 0
  ) |>
  group_by(game_id) |>
  summarize(
    is_cc = any(cc),
    is_aw = any(aw)
  )

wnba_gl <- wnba_gl |>
  left_join(specific_players, by = join_by(game_id)) |>
  mutate(
    is_cc = if_else(is.na(is_cc), 0, is_cc),
    is_aw = if_else(is.na(is_aw), 0, is_aw)
  )



write_rds(wnba_gl, here::here("data/wnba_gl.rds"))


