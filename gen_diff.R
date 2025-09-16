library(tidyverse)
library(ggh4x)
library(showtext)
library(scales)
### Fonts
font_add_google("Noto Sans", "noto")
font_add_google("Roboto Slab", 'rs')
showtext_auto()

## Data
df <- read_csv("D:/VIS/data_makeovermonday/makeovermonday-do-younger-britons-see-kidult-hobbies-as-less-childish/makeovermonday-do-younger-britons-see-kidult-hobbies-as-less-childish/data/data_hodhf.csv")
## Data Wrangling
df_long <-  df |> 
  filter(age_group %in% c('18-34', '65+')) |> 
  pivot_longer(!age_group, 
               names_to = 'activity', 
               values_to = 'percent') 
df_for_plot <- df_long |> 
  pivot_wider(names_from = age_group, values_from = percent) |> 
  rename( young = `18-34`, 
          old = `65+`)
df_for_plot <- df_for_plot |> 
  mutate(activity = str_replace_all(activity, "_", " "), 
         activity = str_to_title(activity) |> 
           str_replace("D D", "(D&D)") |>
           str_replace("Dungeons And Dragons", "Dungeons & Dragons"))

ggplot(df_for_plot) +
  geom_segment(aes(x = old, xend = young, y = activity))+
  geom_point(aes(x = young, y = activity), color = 'indianred', size = 3) +
  geom_point(aes(x = old, y = activity), color = 'lightblue', size =3) +
  scale_x_continuous(labels = scales::percent_format(scale = 1))+
  labs(x = '', 
       y = '', 
       title = 'How Generations Differ in Their Views of Kids’ Activities', 
       caption = 'Data: YouGov | Vis: MhKirmizi') +
  cowplot::theme_minimal_hgrid() +
  theme(
    text = element_text(size = 14, family = 'noto'), 
    plot.title = element_text(size = 36, family = 'rs', 
                              face = 'bold', hjust = .5), 
    plot.caption = element_text(size = 12, family = 'noto'),
    panel.grid.major.y = element_line(linetype = 'dotted'), 
    plot.background = element_rect(fill = 'white', color = 'white'), 
    panel.background = element_rect(fill = 'white', color = 'white')
  )

ggsave("gen_diff.png", width = 1920, height = 1080, units = "px", dpi = 132)

