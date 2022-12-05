library('tidyverse')

topFranchise <- function(n, prob) {
  teams <- c('ATL', 'BOS', 'BKN', 'CHA', 'CHI', 'CLE', 'DAL', 'DEN', 'DET', 'GSW', 'HOU', 'IND', 'LAC', 'LAL', 'MEM', 'MIA', 'MIL', 'MIN', 'NOP', 'NYK', 'OKC', 'ORL', 'PHI', 'PHX', 'POR', 'SAC', 'SAS', 'TOR', 'WAS')
  odds <- as.numeric(c(0.14, 0.14, 0.14, 0.125, 0.105, 0.09, 0.075, 0.06, 0.045, 0.03, 0.02, 0.015, 0.01, 0.005))
  
  topXOdds <- c()
  
  for(pos in 1:14) {
    total = 0
    order <- sample(teams, 13)
    
    # Pre-Lotto Order
    if(pos == 1) {
      order_plus <- c('UTA', order)
    } else if(pos == 14) {
      order_plus <- c(order, 'UTA')
    } else {
      order_plus <- c(order[1:(pos-1)], 'UTA', order[(pos):length(order)])
    }
    
    # Post Lotto Order Simulation
    for(i in 1:1000) {
      official <- sample(order_plus, 4, prob = odds)
      official <- c(official, setdiff(order_plus, official))
      
      if('UTA' %in% official[1:n]) {
        total = total + 1
      }
    }
    
    topXOdds[pos] = total / 1000
  }
  
  data.frame(
    pick = as.factor(seq(1, 14)), 
    franchise_odds = topXOdds
  ) %>%
    ggplot(aes(pick, franchise_odds)) +
    geom_bar(stat = "identity", fill = "#f6ee26") +
    geom_hline(yintercept = prob, color = "#DBE2EA", linetype = "dashed", size = 1.75) +
    scale_y_continuous(labels = scales::label_number(suffix = "%", scale = 1e2)) +
    labs(
      title = "Drafting a Franchise Player", 
      subtitle = paste("Probability by Pick Assuming", n, "Franchise Players in Draft"), 
      y = "Probability", 
      x = "Pre-Lottery Pick Order"
    ) +
    theme_minimal() +
    theme(
      text = element_text(color = "#FFFFFF"),
      axis.text = element_text(color = "#FFFFFF"), 
      panel.grid = element_line(color = "#585858"), 
      plot.background = element_rect(fill = "#050505", color = NA)
    )
}


topFranchise(5, 0.5)





