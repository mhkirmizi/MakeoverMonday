library(tidyverse)
library(gapminder)
library(lemon)
library(showtext)
### 
font_add_google("Montserrat", "mont")
showtext_auto()
### Data
df2 <- gapminder |> 
  filter(year == '2007')
### Plotting
ggplot(df2, aes(x = continent, y = round(lifeExp),1)) +
  geom_point(position = position_jitter(width = .1), 
             aes(size = gdpPercap), alpha = .8) +
  coord_flex_cart(bottom=brackets_horizontal()) +
  labs(
    x = "", 
    y = "Life Expectancy", 
    title = "Higher GDP per Capita, Longer Lives: Exploring Global Age Patterns", 
    subtitle = "A global distribution of life expectancy reveals a positive correlation between life expectancy and GDP per capita.",
    caption = "Data:{gapminder} | Vis: MhKirmizi "
  ) +
  theme_classic(base_family = "mont", base_size = 16) + 
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 36, hjust= .5, face = "bold", colour ="#2a475e"), 
    plot.subtitle = element_text(size = 24, hjust = .5),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )
ggsave("life_expectancy.png", width = 1920, height = 1080, units = "px", dpi = 132)
