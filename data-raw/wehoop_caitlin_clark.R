
library(tidyverse)
library(wehoop)

## Finding games with Caitlin Clark:

wnba_gamelogs_cc <- wnba_playergamelogs(
  player_id = 1642286,
  seasons = 2024:2025
) |>
  pluck(1)

write_rds(wnba_gamelogs_cc, here::here("data/wnba_gamelogs_cc.rds"))
