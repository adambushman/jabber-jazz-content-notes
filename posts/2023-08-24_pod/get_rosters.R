library('tidyverse')
library('rvest')





getRoster <- function(abbr) {
  page = glue::glue("https://www.basketball-reference.com/teams/{abbr}/2024.html")
  
  roster <- 
    read_html(page) %>%
    html_element("#div_roster table") %>%
    html_table() %>%
    janitor::clean_names() %>%
    select(-c(no, x, college, ht, wt))
  
  return(roster)
}



teams <- tibble(
  team = c(
    "ATL", "BOS", "BRK", "CHO", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM",
    "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS"
  )
)

rosters <- teams %>%
  mutate(roster = map(team, getRoster))
