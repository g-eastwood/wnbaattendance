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
  ) |>
  mutate(is_cc = if_else(is.na(is_cc), 0, is_cc))






write_rds(wnba_gl, here::here("data/wnba_gl.rds"))



# Consider this alternative model specification
mod_attendance <- lm(
  attendance ~ rolling_wins_44 + factor(wday(GAME_DATE)) + factor(month(GAME_DATE)) + TEAM_NAME + SEASON_ID + is_cc,
  data = wnba_gl
)

summary(mod_attendance)

