
library(tidyverse)
library(wehoop)

## Finding games with Caitlin Clark:

seasons <- 2024:2025

wnba_gamelogs_cc <- seasons |>
  map(~(wehoop::wnba_playergamelogs(player_id = 1642286, season = .x))) |>
  map(pluck(1)) |>
  list_rbind() |>
  janitor::clean_names()

## Finding games with A'ja Wilson

seasons <- 2018:2025

wnba_gamelogs_aw <- seasons |>
  map(~(wehoop::wnba_playergamelogs(player_id = 1628932, season = .x))) |>
  map(pluck(1)) |>
  list_rbind() |>
  janitor::clean_names()


wnba_gamelogs_cc |>
  bind_rows(wnba_gamelogs_aw) |>
  write_rds(here::here("data/wnba_gamelogs_cc.rds"))
