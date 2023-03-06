library('tidyverse')
library('hoopR')

# Load NBA schedule
all_schedule <- hoopR::load_nba_schedule()

# Transform to a workable format
mutated_sched <- 
  all_schedule %>% 
  mutate(date = lubridate::ymd_hm(date)) %>% 
  filter(id != 401468924) %>% # Removing the postponed DET-WAS game
  filter(!(home_abbreviation %in% c("GIA", "LEB"))) %>% # Removing the All-Star game
  arrange(date) %>%
  select(id, date, home_abbreviation, away_abbreviation, home_winner, away_winner) %>%
  pivot_longer( # Going from 1 row = 1 game to 1 row 1 team game
    !c(date, id, home_winner, away_winner), 
    names_to = "team_col", 
    values_to = "team"
  ) %>%
  mutate( # Identifying which team won
    winner = case_when(
      stringr::str_detect(team_col, "home") & home_winner == TRUE ~ 1, 
      stringr::str_detect(team_col, "away") & home_winner == TRUE ~ 0,
      stringr::str_detect(team_col, "home") & away_winner == TRUE ~ 0,
      stringr::str_detect(team_col, "away") & away_winner == TRUE ~ 1
    )
  ) %>%
  select(id, date, team, winner)

# Defining the legs for a single team's season

legs <- 
  sort(c(rep(seq(1, 7), 10), rep(8, 12)))

# Logic for total wins in a leg to points

getPoints <- function(wins) {
  case_when(
    wins < 3 ~ wins, 
    wins == 3 ~ 4, 
    wins == 4 ~ 6, 
    wins == 5 ~ 9, 
    wins == 6 ~ 12, 
    wins == 7 ~ 16, 
    wins == 8 ~ 20, 
    wins == 9 ~ 25, 
    wins == 10 ~ 30, 
    wins == 11 ~ 36, 
    wins == 12 ~ 42
  )
}

# Conference definition

east = c(
  "MIL", "BOS", "PHI", "CLE", "BKN", "NY", "MIA", "DET", 
  "ATL", "TOR", "WSH", "IND", "CHI", "ORL", "CHA"
)

# Creating the standings

standings <- 
  mutated_sched %>%
  arrange(team, date) %>%
  mutate(leg = as.character(rep(legs, 30))) %>%
  filter(!is.na(winner)) %>%
  group_by(team, leg) %>%
  summarise(wins = sum(winner)) %>%
  ungroup() %>%
  mutate(
    points = getPoints(wins)
  ) %>%
  group_by(team) %>%
  summarise(
    total_points = sum(points)
  ) %>%
  ungroup() %>%
  mutate(conf = case_when(team %in% east ~ "EAST", TRUE ~ "WEST")) %>%
  arrange(desc(total_points)) 

# Eastern Standings

standings %>% 
  filter(conf == "EAST") %>%
  print(n = nrow(.))

# Western Standings

standings %>% 
  filter(conf == "WEST") %>%
  print(n = nrow(.))