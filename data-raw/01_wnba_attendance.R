
library(tidyverse)

att_path <- here::here("data-raw/wnba_attendance")

att_files <- list.files(att_path, pattern = "\\.csv$", full.names = TRUE)

attendance_raw <- map_dfr(att_files, read_csv)

attendance_clean <- attendance_raw |>
  janitor::clean_names() |>
  mutate(
    game_date = as.Date(date, format = "%B %d, %Y"),
    attendance = as.integer(attendance)
  )




# They were the San Antonio Stars at home...
attendance_clean |>
  filter(str_detect(team, "San Antonio")) |>
  group_by(season = year(game_date), team) |>
  count()

# But the San Antonio Silver Stars on the road...
attendance_clean |>
  filter(str_detect(opponent, "San Antonio")) |>
  group_by(season = year(game_date), opponent) |>
  count()

attendance_clean <- attendance_clean |>
  mutate(
    team = ifelse(year(game_date) >= 2003 & year(game_date) <= 2013 & team == "San Antonio Stars", "San Antonio Silver Stars", team)
  )



## Arena capacity

arenas <- read_csv(here::here("data-raw/arenas.csv"))

wnba_attendance <- attendance_clean |>
  left_join(arenas, by = "arena")

wnba_attendance |>
  filter(is.na(arena_capacity)) |>
  group_by(arena) |>
  count() |>
  arrange(desc(n))

wnba_attendance |>
  group_by(cbsa_id) |>
  summarize(
    num_games = n(),
    num_arenas = n_distinct(arena),
    arenas = paste(unique(arena), collapse = "|")
  ) |>
  arrange(desc(num_arenas))

# Gemini consolidated
wnba_attendance <- wnba_attendance |>
  mutate(arena_consolidated = case_when(
    arena %in% c("US Airways Center", "Talking Stick Resort Arena", "Phoenix Suns Arena", "PHX Arena", "Footprint Center") ~ "Footprint Center",
    arena %in% c("Staples Center", "STAPLES Center", "Crypto.com Arena") ~ "Crypto.com Arena",
    arena %in% c("Conseco Fieldhouse", "Bankers Life Fieldhouse", "Gainbridge Fieldhouse") ~ "Gainbridge Fieldhouse",
    arena %in% c("KeyArena", "Climate Pledge Arena") ~ "Climate Pledge Arena",
    arena %in% c("Verizon Center", "Capital One Arena") ~ "Capital One Arena",
    arena %in% c("Time Warner Cable Arena", "Spectrum Center") ~ "Spectrum Center",
    arena %in% c("Philips Arena", "State Farm Arena") ~ "State Farm Arena",
    arena %in% c("Mandalay Bay Events Center", "Michelob ULTRA Arena") ~ "Michelob ULTRA Arena",
    arena %in% c("CareFirst Arena", "CFG Bank Arena") ~ "CFG Bank Arena",
    TRUE ~ arena
  ))

write_rds(wnba_attendance, file = "data/wnba_attendance.rds", compress = "xz")
