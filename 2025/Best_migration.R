library(tidyverse)
library(dlookr)
library(showtext)
### Fonts
font_add_google("Lato", "lato")
showtext_auto()
### Data Wranngling
df_long <- df |> 
  pivot_longer(cols = -Country,   # keep 'Country' as is; pivot everything else
               names_to = "Category",
               values_to = "Score")
ggplot(df_long, aes(Category, Score, group = Country)) +
  geom_line() +
  geom_point() +
  labs(x = "", 
       y = "",
       title = "Best Migration Destination in 2025", 
       subtitle = "People migrate for many reasons, but data from 16 countries shows that economics reasons is often the main one.",
       caption = "Data:Henley & Partners | Vis: MhKirmizi") +
  facet_wrap(~Country) +
  theme_minimal(base_family = 'lato', base_size = 14) +
  theme(
    plot.title = element_text(size= 36, hjust = .5, face = "bold", color = "#2a475e"),
    plot.subtitle = element_text(size = 20, hjust = .5, color = "#1b2838" ),
    axis.text.x = element_text(angle = 90, hjust = 1, colour = "#1b2838"), 
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"), 
    axis.line = element_line(color = "grey50")
  )
ggsave("Best_migration.png", width = 1920, height = 1080, units = "px", dpi = 132)
