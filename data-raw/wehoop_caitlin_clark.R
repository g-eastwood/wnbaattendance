
library(tidyverse)
library(wehoop)

## Finding games with Caitlin Clark and A'ja Wilson.
##
## Output: data/wnba_gamelogs_cc.rds -- one row per player per game played.
## combine.R uses it to flag games with Clark (is_cc) or Wilson (is_aw); it only
## reads player_name, min and game_id.
##
## Two ways to build it:
##   * rebuild_from_api = TRUE : pull every season from the stats.wnba.com API
##     (UNTESTED here -- the API was unreachable). game_id values are NBA-style.
##   * rebuild_from_api = FALSE: keep the saved file and replace one season
##     (update_season) with ESPN data from wehoop::load_wnba_player_box().
##     game_id values for that season are ESPN IDs, which match the game_id used
##     for the same season by wehoop_gamelogs.R.

rebuild_from_api <- FALSE
update_season <- 2026

in_path  <- here::here("data/wnba_gamelogs_cc.rds")
out_path <- here::here("data/wnba_gamelogs_cc.rds")


# 1a. Full rebuild from the stats.wnba.com API

if (rebuild_from_api) {

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
    write_rds(out_path)
}


# 1b. Replace one season using ESPN data (load_wnba_player_box)

if (!rebuild_from_api) {

  existing <- read_rds(in_path)
  # Drop any earlier version of the season so this script can be rerun safely
  existing <- existing |> filter(season_year != as.character(update_season))

  # Regular season only (season_type 2), matching the game logs; players who did
  # not play are left out, as in the API data
  new_rows <- load_wnba_player_box(seasons = update_season) |>
    filter(
      season_type == 2,
      str_detect(athlete_display_name, "^Caitlin Clark$|^A'ja Wilson$"),
      !did_not_play,
      minutes > 0
    ) |>
    transmute(
      season_year = as.character(season),
      player_id = as.character(athlete_id),   # ESPN ID, not the stats.wnba.com ID
      player_name = athlete_display_name,
      team_name = team_display_name,
      game_id = as.character(game_id),
      game_date = format(game_date, "%Y-%m-%dT00:00:00"),
      min = as.character(minutes)
    )

  stopifnot(nrow(new_rows) > 0)
  message(
    "Added ", nrow(new_rows), " player-games for ", update_season, ": ",
    paste(names(table(new_rows$player_name)), table(new_rows$player_name), collapse = ", ")
  )

  # Columns not listed above (shooting, ranks, ...) are NA for the new season;
  # combine.R does not use them
  bind_rows(existing, new_rows) |>
    write_rds(out_path)
}
