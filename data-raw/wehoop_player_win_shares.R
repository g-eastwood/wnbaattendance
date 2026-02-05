# computing win shares (by game)

## offensive contribution:

library(tidyverse)
player_games_clean <- read_rds(here::here("data/player_games_2024.rds"))

player_games_clean <- player_games_clean %>%
  mutate(
    FGA = as.numeric(FGA),
    FTA = as.numeric(FTA),
    TOV = as.numeric(TOV),
    PTS = as.numeric(PTS),
    AST = as.numeric(AST)
  )

player_games_clean <- player_games_clean %>%
  mutate(
    off_poss = FGA + 0.44 * FTA + TOV
  )

player_games_clean <- player_games_clean %>%
  mutate(
    points_produced = PTS + 0.5 * AST
  )

league_ppp <- player_games_clean %>%
  group_by(SEASON_YEAR) %>%
  summarise(
    league_ppp = sum(PTS) / sum(off_poss),
    .groups = "drop"
  )

player_games_clean <- player_games_clean %>%
  left_join(league_ppp, by = "SEASON_YEAR") %>%
  mutate(
    marginal_offense = points_produced - league_ppp * off_poss
  )

## defensive contribution:


player_games_clean <- player_games_clean %>%
  mutate(
    STL = as.numeric(STL),
    BLK = as.numeric(BLK),
    DREB = as.numeric(DREB),
    MIN = as.numeric(MIN)
  )

player_games_clean <- player_games_clean %>%
  mutate(
    defensive_index = STL + BLK + 0.5 * DREB
  )

player_season_2024 <- player_games_clean %>%
  filter(SEASON_YEAR == "2024") %>%
  group_by(PLAYER_ID, PLAYER_NAME, TEAM_ID, TEAM_NAME) %>%
  summarise(
    marginal_offense = sum(marginal_offense, na.rm = TRUE),
    defensive_index  = sum(defensive_index, na.rm = TRUE),
    minutes          = sum(MIN, na.rm = TRUE),
    .groups = "drop"
  )

player_ws_2024 <- player_season_2024 %>%
  group_by(TEAM_ID) %>%
  mutate(
    team_offense = sum(marginal_offense[marginal_offense > 0], na.rm = TRUE),
    team_defense = sum(defensive_index, na.rm = TRUE),

    off_win_share =
      if_else(marginal_offense > 0,
              marginal_offense / team_offense,
              0),

    def_win_share = defensive_index / team_defense,

    win_share = off_win_share + def_win_share
  ) %>%
  ungroup()


# i need to figure out how to generate team win counts across the season, I could honestly just look them all up...

team_wins_2024 <- tibble::tribble(
  ~team_name,              ~team_wins,
  "New York Liberty",      32,
  "Minnesota Lynx",        30,
  "Connecticut Sun",       28,
  "Las Vegas Aces",        27,
  "Seattle Storm",         25,
  "Indiana Fever",         20,
  "Phoenix Mercury",       19,
  "Atlanta Dream",         15,
  "Washington Mystics",    14,
  "Chicago Sky",           13,
  "Dallas Wings",          9,
  "Los Angeles Sparks",    8
)

team_wins_2024 <- team_wins_2024|>
  mutate(
    team_name = toupper(team_name)
  )

player_ws_2024 <- player_ws_2024 %>%
  left_join(team_wins_2024, by = join_by("TEAM_NAME" == "team_name")) |>
  mutate(
    player_win_shares = win_share * team_wins
  )


write_rds(player_ws_2024, here::here("data/player_ws_2024.rds"))



