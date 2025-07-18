library(tidyverse)
library(ggrepel)
library(showtext)
## Fonts
font_add_google(name = "Montserrat", family = "montserrat")
font_add_google(name = "Fira Sans", family = "fira")
showtext_auto()
### text
subtitle <- "The global landscape of religiosity reveals a clear divide: countries like Indonesia and Kenya show strong religious identity and daily prayer, \nwhile nations like Sweden and the Netherlands lean secular. Colors serve only to highlight this contrast."
### Data
df <- read_csv("D:/VIS/data_makeovermonday/makeovermonday-2025w25-harry-potter-dataset/Spirituality & Religion - % of adults who.csv")
df <- df |> 
  rename(religious = `Are religiously affiliated`, 
         after_life = `Say there is definitely/probably life after death`, 
         spirits = `Say that parts of nature can have spirits or spiritual energies`, 
         pray = `Say they pray at least daily`, 
         fortune_telle = `Say they consult a fortune teller, horoscope or other way to see the future`)
### Defining the grid
x <- seq(0, 105)
y <- seq(0, 105)
grid <- expand.grid(x =x, y =y)
grid <- grid |> 
  mutate(z = x*y)
df <- df |> 
  mutate(rel_pray = religious * pray)
ggplot() +
  geom_raster(data = grid, aes(x = x, y = y, fill = z), interpolate = TRUE) +
  scale_fill_viridis_c(option = "E") +
  geom_point(data = df, aes(x = religious, y = pray), inherit.aes = FALSE)+
  geom_text_repel(data = df ,size = 6, aes(x = religious, y = pray, label= Country)) +
  scale_x_continuous(limits = c(38, NA), expand = c(0,0))+
  scale_y_continuous(expand = c(.01, 0)) + 
  labs(x =  "Religously affiliated population (%)", 
       y = "Pray at least once in a day (%)",
       title = "The Global Faith Split", 
       subtitle = subtitle,
       caption = "Data:{Pew Research Center} | Vis: MhKirmizi") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 44, hjust = .5, face = "bold", family = "montserrat"), 
    plot.subtitle = element_text(size = 20, hjust = .5, family = 'fira'),
    plot.caption = element_text(size = 14),
    plot.background = element_rect(fill = "#F4F5F1", colour = "#F4F5F1"), 
    panel.background = element_rect(fill = "#F4F5F1", colour = "#F4F5F1")
  ) 
ggsave("religous.png", width = 1920, height = 1080, units = "px", dpi = 132) 
  
  
