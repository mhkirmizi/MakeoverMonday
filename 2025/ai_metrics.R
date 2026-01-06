library(tidyverse)
library(showtext)
library(ggforce)
library(ggtext)
### Fonts
font_add_google("Montserrat", 'mont')
font_add_google("Autour One", 'aut')
showtext_auto()
### Subtitle 
subtitle <- "Should you trust an AI model? Not entirely. Every model exhibits substantial hallucination and still struggles with accuracy."
## text
text_df <- data.frame(
  x = c(1.7, 2.15),
  y = c(.9, .9),
  label = c('Hal.', 'Acc.')
)

### Data wrangling
 df_acc <- df |> 
   select(-`Hallucination Index (Lower is better)`) |> 
   rename(
      accuracy = `Accuracy Index (Higher is better)`
   ) |> 
   mutate(not_acc = 1-accuracy) |> 
   pivot_longer(
     cols = -Model, 
     names_to = 'index', 
     values_to = 'value' 
   ) |> 
   group_by(Model) |> 
   mutate(start_raw = lag(value, default = 0) * pi, 
          end_raw = start_raw +  value*pi, 
          start = start_raw - pi/2,
          end = end_raw - pi /2) 
 ### 
 df_hac <- df |> 
   select( -`Accuracy Index (Higher is better)`) |> 
   rename(hallucination = `Hallucination Index (Lower is better)`) |>
   mutate(not_hac = 1-hallucination) |> 
   pivot_longer(
     cols = -Model, 
     names_to = 'index', 
     values_to = 'value' 
   ) |> 
   group_by(Model) |> 
   mutate(start_raw = lag(value, default = 0) * pi , 
          end_raw = start_raw +  value*pi, 
          start = start_raw - pi/2, 
          end = end_raw - pi/2) 
 
  ggplot() +
  geom_arc_bar(
    data = df_acc,
    aes(
      x0 = 1, 
      y0 =1,
      fill = index,
      start = start, 
      end = end, 
      r0 = 1, 
      r= 1.25
    )
  ) +
  geom_arc_bar(
    data = df_hac,
    aes(
      x0 = 1, 
      y0 =1,
      fill = index,
      start = start, 
      end = end, 
      r0 = .5, 
      r= .75
    ) 
  ) +
  geom_text(
    data = text_df, 
    mapping = aes(x = x, y = y, label = label),
    size = 4
  )+
  labs(
    title = 'Metrics of AI Models: Accuracy & Hallucuniation', 
    subtitle = subtitle,
    caption = 'Data Source: Voronoi | Vis: MhKirmizi'
  ) +
  scale_fill_manual(values = c('accuracy' = '#94475EFF', 'hallucination' = '#E5A11FFF', 
                    'not_acc' =  '#364C54FF', 'not_hac' = '#364C54FF')) +
  coord_fixed(ratio = 1.5) +
  facet_wrap(~Model, ncol = 6) +
  theme_void(base_family = 'aut', base_size = 14) +
  theme(legend.position = 'none', 
        plot.title.position = 'plot',
        plot.title = element_text(size = rel(1.5),
                                  hjust = .5,
                                  vjust = .5,
                                  family = 'mont', 
                                  face = 'bold', 
                                  margin = margin(b = 5)), 
        plot.subtitle = element_textbox_simple(hjust = .5, 
                                               vjust = .5, 
                                               halign = 0,
                                               margin = margin(b = 5),
                                               size =rel(1.2)), 
        plot.background = element_rect(colour = 'white', fill = 'white'),
        panel.background = element_rect(color= 'white', fill = 'white'),
        margins = margin(10, 10, 10, 10)
    )
  ggsave("ai_metrics.png", width = 1920, height = 1080, units = "px", dpi = 132)  
  

    