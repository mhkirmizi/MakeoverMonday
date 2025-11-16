library(tidyverse)
library(sf)
library(geojsonio)
library(showtext)
## Getting the fonts
font_add_google("EB Garamond", 'eb')
font_add_google('Roboto', 'robo')
showtext_auto()
### Getting the hexbin data for the map
us_map <- read_sf("D:/VIS/hexbin/us_states_hexgrid.geojson")

us_map <- us_map %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
us_map <- us_map |> 
  left_join(df, by = c('google_name' = 'State' ))

### Binning
us_map <- us_map |> 
  mutate(clss = case_when(
    `Living WWII Veterans (2025)` < 100 ~ '<100', 
    `Living WWII Veterans (2025)` < 500 ~ '<500', 
    `Living WWII Veterans (2025)` < 1000 ~ '<1000', 
    `Living WWII Veterans (2025)` < 2000 ~ '<2000', 
    `Living WWII Veterans (2025)` < 3000 ~ '<3000', 
    TRUE ~ '3000+'
  )) |> 
  mutate(
    clss = factor(
      clss,
      levels = c("<100", "<500", "<1000", "<2000", "<3000", "3000+")))
      

### Let's use ggplot
us_map|> ggplot() +
  geom_sf(aes(fill = clss),linewidth = 0, alpha = .9) +
  geom_sf_text(aes(label = iso3166_2), color = 'white', size = 5, alpha = .9)+
  labs(
    title = 'Number of Living WWII Veterans in Each State', 
    caption = "Data: makeovermonday |  Vis: MhKirmizi", 
    fill = 'Numbers in 2025'
  ) +
  paletteer::scale_fill_paletteer_d("ggsci::dark_uchicago")+
  guides(fill = guide_legend(nrow = 1)) +
  theme_void(base_size = 14, base_family = 'robo') +
  theme(
    legend.position = c(0.5, 0.93),
    legend.title = element_text(hjust = .5),
    plot.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
    panel.background = element_rect(fill = "#f5f5f2", color = "#f5f5f2"), 
    plot.title = element_text(size = 32, hjust = .5, face = 'bold', family = 'eb'), 
    plot.caption = element_text(size = 14), 
    plot.margin = margin(15, 10, 15,10)
  )
ggsave("week44.png", width = 1632, height = 918, units = "px", dpi = 132)
