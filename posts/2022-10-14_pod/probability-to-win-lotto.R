new_odds <- as.numeric(c(0.14, 0.14, 0.14, 0.125, 0.105, 0.09, 0.075, 0.06, 0.045, 0.03, 0.02, 0.015, 0.01, 0.005))
old_odds <- as.numeric(c(0.25, 0.199, 0.156, 0.119, 0.088, 0.063, 0.043, 0.028, 0.017, 0.011, 0.008, 0.007, 0.006, 0.005))

data.frame(
  odds_rank = factor(rep(seq(1:14), 2), levels = seq(1:14)),
  odds = c(new_odds, old_odds),
  type = c(rep("New", 14), rep("Old", 14))
) %>%
  ggplot(
    .,
    aes(odds_rank, odds, color = type)
  ) +
  geom_point(
    size = 3
  ) +
  labs(
    title = "Probability to Win Lottery",
    subtitle = "Pre-2017 Change (Old) vs Since-2017 Change (New)",
    y = "Probability to",
    x = "Pre-Lottery Pick Order"
  ) +
  scale_y_continuous(labels = scales::label_number(suffix = "%", scale = 1e2)) +
  scale_color_manual(values = c("#f6ee26", "#dbe2ea")) +
  theme_minimal() +
  theme(
    text = element_text(color = "#FFFFFF"),
    axis.text = element_text(color = "#FFFFFF"),
    panel.grid = element_line(color = "#585858"),
    plot.background = element_rect(fill = "#050505", color = NA)
  )

camcorder::gg_record(
  dir = 'C:/Users/Adam Bushman/Pictures/_test',
  device = 'png',
  width = 16,
  height = 9,
  units = 'cm',
  dpi = 300
)

