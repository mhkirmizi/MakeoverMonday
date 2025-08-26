library(tidyverse)
library(waffle)
library(showtext)
## 
font_add_google("Staatliches","sta")
font_add_google("Raleway","ral")
showtext_auto()

### Data Wrangling
df <- read_excel("D:/VIS/data_makeovermonday/Global meat production by livestock type.xlsx")
df |> glimpse()
df <- df |> 
  rename(
  game  =  `Meat, game | 00001163 || Production | 005510 || tonnes`,
  horse = `Meat, horse | 00001097 || Production | 005510 || tonnes` , 
  camel = `Meat, camel | 00001127 || Production | 005510 || tonnes`, 
  sheep_goat = `Meat, sheep and goat | 00001807 || Production | 005510 || tonnes`, 
  beef_buffalo = `Meat, beef and buffalo | 00001806 || Production | 005510 || tonnes`, 
  pork = `Meat, pig | 00001035 || Production | 005510 || tonnes`,
  poultyr = `Meat, poultry | 00001808 || Production | 005510 || tonnes`
  )
df <- df |> 
  replace_na(
    list(
      game = 0,
      horse = 0,
      camel = 0,
      sheep_goat = 0,
      beef_buffalo = 0,
      pork = 0,
      poultyr =0
    )
  )
### color based on the different animals.
df_continent <- df |> 
  filter(is.na(Code))
unique(df_continent$Entity)
df_continent_total <- df_continent |> 
  filter(Entity %in% c('Africa', 'Europe', 'South America', 'North America', 'Asia')) |> 
  filter(Year ==2023) |> 
  mutate(
    game =as.integer(round(game / 10000000)), 
    horse = as.integer(round(horse / 10000000)), 
    camel = as.integer(round(camel / 10000000)),
    sheep_goat = as.integer(round(sheep_goat / 10000000)), 
    beef_buffalo = as.integer(round(beef_buffalo / 10000000)), 
    pork = as.integer(round(pork / 10000000)), 
    poultyr = as.integer(round(poultyr / 10000000)),
    total = game + horse + camel + sheep_goat + beef_buffalo + pork + poultyr
  )
### Pivoting data
df_continent_total |> 
  pivot_longer(
    cols = c(game, horse, camel, sheep_goat, beef_buffalo, pork, poultyr),
    names_to = 'meat', 
    values_to = 'tonnes'
  ) |> 
ggplot(aes(fill = meat)) +
  geom_waffle(
    aes(values = round(total)), # number of squares per meat
    n_rows = 4,
    size = 0.75,
    color = "white",
    flip = FALSE
  ) +
  facet_wrap(~Entity, ncol = 1, strip.position = "left") +
  coord_equal() +
  scale_fill_brewer(palette = 'Set2', 
                    labels = c("game" = "Game Meat","horse" = "Horse","camel" = "Camel", 
                               "sheep_goat" = "Sheep & Goat","beef_buffalo" = "Beef & Buffalo",
                               "pork" = "Pork","poultyr" = "Poultry")) +
    labs(y = "", 
         x = "", 
         fill = "", 
         title = "Global Meat Production", 
         subtitle = "The waffle chart below shows the global meat production, with each square representing 1 million cases.", 
         caption = "Data: Our World In Data | Vis: MhKirmizi") +
  theme_void(base_family = 'ral', base_size = 14) +
  theme(
    legend.position = 'right',
    axis.ticks.x = element_blank(), 
    axis.text = element_blank(), 
    plot.title = element_text(size = 36, hjust = .5, face = 'bold', family = 'sta'),
    plot.subtitle = element_text(size = 18, hjust = .5),
    plot.background = element_rect(fill = 'white', color = 'white'),
    panel.background = element_rect(fill = 'white', color = 'white'),
    margin(t= 10, b= 5, r=5, l=5)
  )
ggsave("meat.png", width = 1920, height = 1080, units = "px", dpi = 132)



