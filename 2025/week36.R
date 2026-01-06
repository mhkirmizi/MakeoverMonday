library(tidyverse)
library(ggradar)
library(showtext)
library(patchwork)
###
font_add_google('Roboto', 'robo')
font_add_google("Lobster Two", "lobstertwo")
showtext_auto()

df_long <- df |> 
  pivot_longer(
    -c('category', 'criteria', 'group'),
    names_to = 'drug', 
    values_to = 'harm_percent'
  )

p <- df_long |> 
  filter(group == 'Harm to others', 
         drug  == 'alcohol'
         ) |> 
  select(drug, criteria, harm_percent) |> 
  pivot_wider(names_from = 'criteria', 
              values_from = 'harm_percent') |> 
  ggradar(axis.label.size = 4, 
          grid.label.size = 5,
          font.radar = 'roboto',
          group.point.size = 3
          ) +
  labs(title = 'Harms of Alcohol to Others') +
  theme(
    plot.title = element_text(size = 24, hjust = .5, 
                              family = "lobstertwo", 
                              colour = "#2a475e",
                              face = 'bold'),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )
  

q <- df_long |> 
  filter(group == 'Total') |> 
  select(drug, criteria, harm_percent) |> 
  mutate(
    drug = str_to_title(drug),
    drug = reorder(drug, harm_percent),
    drug_fill = factor(ifelse(drug == 'Alcohol', 1, 0))) |> 
  ggplot(aes(x = drug, y = harm_percent)) +
  geom_col(aes(fill = drug_fill)) +
  paletteer::scale_fill_paletteer_d("ggthemes::wsj_dem_rep", direction =-1)+
  coord_flip() +
  labs(x = '', 
       y = 'Harm(%)', 
       title = 'Harm of Different Substances') +
  theme_minimal(base_family = 'roboto') +
  theme(
    plot.title = element_text(size = 24, hjust = .5, 
                              family = "lobstertwo", 
                              colour = "#2a475e",
                              face = 'bold'),
    legend.position = 'none',
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )
### use patchwork to combine these three together the story data tells why alcohol is legal

q + p +plot_annotation(title = 'Alcohol vs. Other Drugs: Relative Harm', 
                       caption = 'Data: The Lancet | Vis: MhKirmizi', 
                       theme = theme(plot.title = element_text(size = 48, 
                                                    hjust = .5, 
                                                    family = "lobstertwo", 
                                                    face = 'bold', 
                                                    colour = "#09283CFF"), 
                                     plot.caption = element_text(size = 12, family = 'robo'), 
                                     plot.background = element_rect(fill = "#fbf9f4", 
                                                                    color = "#fbf9f4"),
                                     panel.background = element_rect(fill = "#fbf9f4",
                                                                     color = "#fbf9f4")
                                     ))
ggsave("week36.png", width = 1920, height = 1080, units = "px", dpi = 132)
