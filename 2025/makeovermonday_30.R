library(tidyverse)
library(ggridges)
library(showtext)
## Fonts
font_add_google("Montserrat", "Montserrat")
showtext_auto()

### Text
title <- 'Victoria Line Is Getting Warmer: A Decade of Rising Temperatures'

### Data Wrangling and Plotting
df <- read_csv("D:/VIS/data_makeovermonday/2025_w30.csv")
df <- df |> 
  select(-c(`Sub-surface_lines`,...11, ...12,...13))

df |> 
  pivot_longer(!c(Year, Month), names_to = 'city', values_to = 'temp') |> 
  filter(city == 'Victoria') |> 
  ggplot(aes(temp, as.factor(Year), fill = stat(x))) +
  geom_density_ridges_gradient(rel_min_height = 0.01, scale = 1.75) +
  scale_fill_viridis_c(option = 'B') +
  scale_y_discrete(breaks = seq(2013, 2024,3), label = seq(2013, 2024, 3)) +
  labs(x = 'Temp [C]',
       y = '', 
       title = title, 
       caption = 'Data: TFL| Vis: MhKirmizi', 
       fill = 'Temp' 
       ) +
  theme_ridges() +
  theme(
    text = element_text(size = 14),
    legend.position = 'none', 
    plot.title = element_text(size = 28, hjust = .5, face = 'bold'), 
    panel.background = element_rect(color = 'white', fill = 'white'),
    plot.background = element_rect(color = 'white', fill = 'white')
  )
ggsave("makeovermondway_w30.png", width = 1920, height = 1080, units = "px", dpi = 132) 

