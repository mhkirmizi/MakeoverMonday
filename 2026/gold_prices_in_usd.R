library(tidyverse)
library(showtext)
library(scales)
library(geomtextpath)
### Fonts
font_add_google("EB Garamond", 'eb')
font_add_google('Roboto', 'robo')
showtext_auto()

### Data for era
eras = data.frame(
  start = c(as.Date('2006-05-22'), as.Date('2011-08-24'), as.Date('2022-03-29')),
  end = c(as.Date('2011-07-25'), as.Date('2022-02-27'), as.Date('2026-01-07')),
  era = c("Steady Increase", "Stagnation", "Skyrocketing")
)

### Data Wrangling
df |> 
  mutate(Date = as.Date(Date), 
         `Close (kg)` = `Close (kg)` / 1000) |> 
  rename(High = `High (kg)`,
         Low = `Low (kg)`, 
         Close = `Close (kg)`) |> 
  ggplot(aes(Date, Close, group = 1)) +
  geom_area(fill = '#F7C508', alpha = .3) +
  geom_line(color = '#F7C508', linewidth = 1.5) +
  scale_x_date() +
  scale_y_continuous(breaks = seq(50, 170, 50), 
                     labels = label_dollar(suffix = 'K')) +
  annotate("text", x = as.Date('2011-08-24'), y = 101, 
           label = "All time high", vjust = 1, color= "white", size = 6) +
  geom_segment(aes(x = as.Date('2011-07-25'),
                   xend = as.Date('2011-07-25'),
                   y = 62, yend = 95), color = 'grey66', linetype = 3) +
  annotate("text", x = as.Date('2022-02-27'), y = 101, 
           label = "Ukraine War", vjust = 1, color= "white", size = 6) +
  geom_segment(aes(x = as.Date('2022-02-27'),
                   xend = as.Date('2022-02-27'),
                   y = 62, yend = 95), color = 'grey66', linetype = 3) +
  labs(x = '', 
       y = 'Closing Price ($)', 
       title = 'Gold Reacts When the World Breaks', 
       subtitle = 'Gold prices generally rise gradually over time, but global crises trigger sharp and sudden increases.',
       caption = 'Data: MakeoverMonday(2026, w:5) | Vis: MhKirmizi') +
  geom_textsegment(inherit.aes = FALSE, 
                   data = eras, 
                   aes(x = start, xend = end, y= 165, yend = 165, label = era), 
                   color = 'white', family = 'robo', size = 6, 
                   linecolor="grey36") +
  theme_minimal(base_size = 12, base_family = 'robo') +
  theme(
    text = element_text(color = 'white'),
    plot.background = element_rect(fill= 'black', color = NA), 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_line(linetype = 3, color = '#202922'), 
    axis.title.y = element_text(family = 'robo', face = 'bold', size = 12),
    axis.text.y = element_text(size = 12, color = 'white', family = 'robo', face = 'bold'), 
    axis.text.x = element_text(size = 14, color = 'white', 
                               family = 'robo', face = 'bold'),
    plot.title = element_text(size = 36, color = '#CF9B14', 
                              family = 'eb', face = 'bold', hjust = .5), 
    plot.subtitle = element_text(size = 24, family = 'eb', face = 'italic', hjust = .5),
    plot.caption = element_text(family = 'robo', hjust = 0, size = 12), 
    margins = margin(10,10,10,10)
  )
ggsave("week5.png", width = 1920, height = 1080, units = "px", dpi = 132)  
