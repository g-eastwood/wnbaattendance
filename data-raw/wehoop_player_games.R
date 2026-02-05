

##THIS chunk doesn't work
library(wehoop)
library(dplyr)
library(purrr)

teams <- c(
  1611661329, # Las Vegas Aces
  1611661317, # Chicago Sky
  1611661328, # Connecticut Sun
  1611661319, # Dallas Wings
  1611661322, # Indiana Fever
  1611661324, # Los Angeles Sparks
  1611661323, # Minnesota Lynx
  1611661320, # New York Liberty
  1611661325, # Phoenix Mercury
  1611661321  # Seattle Storm
)


all_players <- map_dfr(teams, ~ wnba_franchiseplayers(team = .x)) #%>%
#  select(PERSON_ID, PLAYER) %>%
#  distinct()

library(tidyr)
library(dplyr)

all_players_clean <- all_players %>%
  unnest(FranchisePlayers) %>%
  select(PERSON_ID, PLAYER) %>%
  distinct()

players <- all_players_clean %>%
  rename(PLAYER_ID = PERSON_ID) %>%
  arrange(PLAYER)

library(purrr)
library(wehoop)
library(dplyr)

safe_gamelog <- possibly(
  ~ wnba_playergamelogs(
      player_id = .x,
      season = "2024"   # or most_recent_wnba_season()
    ),
  otherwise = NULL
)

library(progress)

pb <- progress_bar$new(
  format = "Pulling gamelogs [:bar] :current/:total (:percent)",
  total = nrow(players),
  clear = FALSE,
  width = 60
)

player_games <- map_dfr(players$PLAYER_ID, function(pid) {
  pb$tick()
  Sys.sleep(0.6)  # VERY important: prevents rate-limiting
  safe_gamelog(pid)
})


player_games_clean <- player_games %>%
  unnest(PlayerGameLogs) %>%
  mutate(
    GAME_DATE = as.Date(GAME_DATE),
    PLAYER_ID = as.integer(PLAYER_ID)
  )

saveRDS(player_games_clean, "data/player_games_2024.rds")

