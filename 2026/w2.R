library(tidyverse)
library(showtext)
font_add_google('Solway', 'sol')
showtext_auto()

df |> 
  arrange(`Bubble Risk Score`) |> 
  mutate(
    City = factor(City, levels = City),
    label_score = if_else(`Bubble Risk Score` != lag(`Bubble Risk Score`),
    as.character(`Bubble Risk Score`), "")) |> 
  filter(`Bubble Risk Score` > 0) |> 
  ggplot(aes(City, `Bubble Risk Score`)) +
  geom_col(stat = 'identity', aes(fill = `Bubble Risk Category`)) + 
  geom_text(aes(x = City, y = `Bubble Risk Score` + .15,
                label = label_score )) +
  ylim(-.6, 1.9) +
  labs(x = NULL, 
       y = NULL, 
       title = 'The Biggest Real Estate Bubble Risks', 
       caption= 'Data: MakeoverMonday | Vis: MhKirmizi') +
  scale_fill_manual(values = c('Low' = '#E9A800FF', 
                               'Moderate' = '#BF542EFF', 
                               'High' = '#8D3431FF', 
                               'Elevated' = '#5B2125FF'))+
  coord_polar(start = 0) +
  theme_minimal(base_size = 12, base_family = 'sol') +
  theme(
    axis.text.y = element_blank(), 
    axis.text.x = element_text(size = 12, face = 'bold'),
    legend.position = 'top', 
    plot.title = element_text(size = 24, hjust = .5, 
                              face = 'bold', colour = '#241D1DFF'), 
    plot.caption = element_text(hjust = .5), 
    plot.background = element_rect(fill = 'white', color = 'white'), 
    panel.background = element_rect(fill = 'white', color = 'white')
  )
ggsave("W2.png", width = 1920, height = 1080, units = "px", dpi = 132)
