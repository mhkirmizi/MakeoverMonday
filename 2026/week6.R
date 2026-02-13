library(tidyverse)
library(showtext)
library(ggtext)
library(scales)

### Fonts
font_add_google("Roboto Condensed", 'rc')
font_add_google('Roboto', 'rb')
showtext_auto()
### 
g20 <- c('Argentina', 'Australia', 'Brazil', 
         'China', 'Indonesia', 'South Korea', 
         'South Africa', 'Turkey',
         'Canada', 'Japan', 'Britain', 'United States')
### title & subtitle
subtitle <- 'Over 25 years, some countries show stable burger prices, others steady increases, and some extreme volatility revealing very different economic trajectories'
title <- "Stable, Rising, and Wild: A Big Mac Tour of Global Prices"

### Ploting
df_plot <- df |> 
  filter(name %in% g20) |> 
  mutate(year = year(date)) |> 
  group_by(iso_a3, name, year) |> 
  summarise(price = mean(dollar_price), .groups = 'drop') 


df_plot |> 
  ggplot(aes(year, price, group = iso_a3)) +
  geom_line(size = .9)  +
  geom_point() +
  scale_x_continuous(limits = c(2000, 2025), 
                     breaks = seq(2000, 2025, 10)) +
  scale_y_continuous(labels = label_dollar()) +
  labs(x = NULL, 
       y = NULL,
       title = title,
       subtitle = subtitle,
       caption = 'Data: MakeoverMonday, 2025 (Week:5) | Vis: MhKirmizi') +
  facet_wrap(~name,nrow = 4) +
  theme_minimal(base_size = 14, base_family = 'rb') +
  theme(
    plot.title = element_text(size = 28, hjust= 0, 
                                  family = 'rc', face = 'bold', 
                                  color = "#2a475e"), 
    plot.subtitle = element_text(size = rel(1.5), hjust = 0, 
                                 family = 'rc', color = "#2a475e"),
    plot.caption = element_text(size = 14, color = "#1b2838"),
    axis.text.x = element_text(size = 14, family = 'rc', color = "#1b2838"), 
    axis.text.y = element_text(size = 14, family = 'rc', color = "#1b2838"), 
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"), 
    axis.line = element_line(color = "grey50"), 
    strip.background = element_blank(), 
    strip.text = element_text(family = 'rc', 
                              size = rel(1.4),
                              color = "#1b2838"), 
    margins = margin(10, 10, 10, 10))

ggsave("week5.png", width = 1920, height = 1080, units = "px", dpi = 132)  


