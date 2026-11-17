
library(tidyverse)
library(wehoop)

## Wrangling gamelog data from wehoop.
##
## Output: data/wnba_gamelogs.rds -- one row per team per game, in the column
## layout of the stats.wnba.com league game log (read by compute.R).
##
## Two ways to build it:
##   * rebuild_from_api = TRUE : pull every season from the stats.wnba.com API
##     (1997 onward). The API is often unreachable (requests hang, then time out).
##   * rebuild_from_api = FALSE: keep the saved file and replace one season
##     (update_season) with ESPN data from wehoop::load_wnba_team_box().
##     ESPN data starts in 2003, so this is only for recent seasons.

rebuild_from_api <- FALSE
update_season <- 2026

in_path  <- here::here("data/wnba_gamelogs.rds")
out_path <- here::here("data/wnba_gamelogs.rds")


# 1a. Full rebuild from the stats.wnba.com API (UNTESTED -- API was unreachable)

if (rebuild_from_api) {
  options(timeout = 180)

  get_season <- function(s) {
    Sys.sleep(2)
    tryCatch(
      wehoop::wnba_leaguegamelog(season = s, season_type = "Regular Season", league_id = "10")$LeagueGameLog,
      error = function(e) {
        message("Failed: ", s)
        NULL
      }
    )
  }

  gamelogs <- 1997:2026 |>
    map(get_season) |>
    list_rbind() |>
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

  write_rds(gamelogs, file = out_path, compress = "xz")
}


# 1b. Replace one season using ESPN data (load_wnba_team_box)

if (!rebuild_from_api) {

  existing <- read_rds(in_path)
  # Drop any earlier version of the season so this script can be rerun safely
  existing <- existing |> filter(season_id != update_season)

  # Crosswalk: ESPN abbreviation -> the team_abbreviation / team_id used in the
  # saved data. The saved data uses stats.wnba.com codes and IDs, ESPN does not.
  abbr_map <- c(LA = "LAS", LV = "LVA", NY = "NYL", WSH = "WAS", GS = "GSV")

  prior_ids <- existing |>
    filter(season_id == max(season_id)) |>
    distinct(team_abbreviation, team_id)

  xwalk <- tibble(
    espn_abbr = c("ATL", "CHI", "CON", "DAL", "IND", "MIN", "PHX", "SEA", names(abbr_map)),
    team_abbreviation = coalesce(unname(abbr_map[espn_abbr]), espn_abbr)
  ) |>
    left_join(prior_ids, by = "team_abbreviation")

  # Expansion teams have no stats.wnba.com ID in the saved data, so they get a
  # synthetic ID (ESPN's own team ID). Portland's abbreviation is changed from
  # POR to PDX: the old Portland Fire (2000-02) already uses POR, and compute.R
  # keys its Elo ratings by abbreviation, so reusing it would carry over the
  # defunct team's rating. Change these if you want something different.
  new_franchises <- tribble(
    ~espn_abbr, ~team_abbreviation, ~team_id,
    "TOR",      "TOR",              "espn_131935",
    "POR",      "PDX",              "espn_132052"
  )

  xwalk <- bind_rows(xwalk, new_franchises)

  stopifnot(!anyNA(xwalk$team_id), !anyDuplicated(xwalk$espn_abbr))

  # Games to exclude: the Commissioner's Cup final is not part of the regular
  # season in the stats.wnba.com data (2025 has exactly 44 games per team there).
  cup_final <- load_wnba_schedule(seasons = update_season) |>
    filter(notes_headline == "WNBA Commissioner's Cup Championship") |>
    pull(game_id)

  # season_type 2 = regular season (postseason is 3). All-Star teams also show
  # up under 2 (e.g. TEAM SPOON); they are dropped by the inner join below.
  box <- load_wnba_team_box(seasons = update_season) |>
    filter(season_type == 2, !game_id %in% cup_final)

  opp_xwalk <- xwalk |>
    select(opponent_team_abbreviation = espn_abbr, opponent = team_abbreviation)

  # ESPN's own team_id / team_abbreviation are replaced by the crosswalk values
  new_rows <- box |>
    select(-team_id) |>
    rename(espn_abbr = team_abbreviation) |>
    inner_join(xwalk, by = "espn_abbr") |>
    inner_join(opp_xwalk, by = "opponent_team_abbreviation") |>
    mutate(
      season_id = season,
      team_name = team_display_name,
      game_id = as.character(game_id),
      is_home = team_home_away == "home",
      is_away = !is_home,
      matchup = if_else(
        is_home,
        paste0(team_abbreviation, " vs. ", opponent),
        paste0(team_abbreviation, " @ ", opponent)
      ),
      wl = if_else(team_score > opponent_team_score, "W", "L"),
      win_flag = if_else(wl == "W", 1L, 0L),
      # ESPN's percentages are rounded to whole numbers; recompute as proportions
      fg_pct = round(field_goals_made / field_goals_attempted, 3),
      fg3_pct = round(three_point_field_goals_made / three_point_field_goals_attempted, 3),
      ft_pct = round(free_throws_made / free_throws_attempted, 3)
    ) |>
    transmute(
      season_id, team_id, team_abbreviation, team_name, game_id, game_date, matchup, wl,
      min = NA_character_,
      fgm = as.character(field_goals_made),
      fga = as.character(field_goals_attempted),
      fg_pct = as.character(fg_pct),
      fg3m = as.character(three_point_field_goals_made),
      fg3a = as.character(three_point_field_goals_attempted),
      fg3_pct = as.character(fg3_pct),
      ftm = as.character(free_throws_made),
      fta = as.character(free_throws_attempted),
      ft_pct = as.character(ft_pct),
      oreb = as.character(offensive_rebounds),
      dreb = as.character(defensive_rebounds),
      reb = as.character(total_rebounds),
      ast = as.character(assists),
      stl = as.character(steals),
      blk = as.character(blocks),
      tov = as.character(total_turnovers),
      pf = as.character(fouls),
      pts = as.character(team_score),
      plus_minus = as.character(team_score - opponent_team_score),
      video_available = NA_character_,
      win_flag, is_home, is_away, opponent
    )

  # Sanity checks: every game has exactly one home and one away row
  game_check <- new_rows |>
    group_by(game_id) |>
    summarize(n = n(), homes = sum(is_home))
  stopifnot(all(game_check$n == 2), all(game_check$homes == 1))
  stopifnot(identical(names(new_rows), names(existing)))

  message("Added ", nrow(new_rows), " rows (", n_distinct(new_rows$game_id), " games) for ", update_season)
  print(count(new_rows, team_abbreviation, name = "games"), n = Inf)

  gamelogs <- bind_rows(existing, new_rows) |>
    arrange(team_id, game_date)

  write_rds(gamelogs, file = out_path, compress = "xz")
}
