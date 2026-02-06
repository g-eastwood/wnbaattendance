library(tidyverse)

wnba_gamelogs <- read_rds(here::here("data/wnba_gamelogs.rds"))

wnba_gamelogs_home <- wnba_gamelogs |>
  mutate(
    is_away = str_detect(MATCHUP, "@"),
    game_date = as.Date(GAME_DATE)
  ) |>
  filter(!is_away)


# Add attendance to each game

wnba_attendace <- read_csv(here::here("data/wnba_attendance.csv"))

wnba_gl <- wnba_gamelogs_home |>
  left_join(
    wnba_attendace |>
      select(game_date, Team, attendance), 
    by = join_by(game_date, TEAM_NAME == Team)
  )

# Add Caitlin Clark indicator

caitlin_clark <- read_rds(here::here("data/wnba_gamelogs_cc.rds"))

wnba_gl <- wnba_gl |>
  left_join(
    caitlin_clark |> mutate(is_cc = MIN > 0) |> select(GAME_ID, is_cc), 
    by = join_by(GAME_ID)
  )






write_rds(wnba_gl, here::here("data/wnba_gl.rds"))
