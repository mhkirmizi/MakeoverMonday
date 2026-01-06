library(tidyverse)
library(ggridges)
library(showtext)
### Fonts
font_add_google("EB Garamond", 'eb')
font_add_google('Roboto', 'robo')
showtext_auto()
### Data Wrangling
glimpse(df)
df <- df |> 
  mutate(
    Date = lubridate::mdy(Date),
    month = lubridate::month(Date),
    year = lubridate::year(Date))

df <- df |> 
  mutate(`Prime-Age LFPR` = as.numeric(gsub("%", "", `Prime-Age LFPR`)))

df <- df |> 
  rename(Prime_age = `Prime-Age LFPR`)
df <- df |> 
  mutate(year = as.factor(year))

ggplot(df, aes(Prime_age, as.factor(year), fill = as.factor(year))) +
  stat_density_ridges(rel_min_height = 0.01, 
                      quantile_lines = TRUE, 
                      quantiles = 2, 
                      alpha = .7) +
  labs(x = 'Labor Participation Rate (%)', 
       y = '', 
       title = 'Better Things Come To Those Who Wait: Analysis of Labor Participation',
       caption = 'Data: epi.org | Vis:MhKirmizi') +
  scale_fill_viridis_d(option = "C", direction = -1) +
  cowplot::theme_minimal_hgrid() +
  theme(
    text = element_text(size = 12, family = 'robo'),
    panel.grid.major.y = element_line(linetype = 'dotted'),
    plot.title = element_text(size = 32, hjust = .5, family = 'eb', face = 'bold'),
    plot.background = element_rect(fill = "#F1F4EEFF", color = "#F1F4EEFF"), 
    panel.background = element_rect(fill = "#F1F4EEFF", color = "#F1F4EEFF"),
    legend.position = 'none',
  )
ggsave("week40.png", width = 1632, height = 918, units = "px", dpi = 132)

