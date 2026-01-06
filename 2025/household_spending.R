library(tidyverse)
library(ggh4x)
library(showtext)
## Fonts
font_add_google('Roboto', 'robo')
font_add_google("Montserrat", 'mons')
showtext_auto()
### to do list 
## fix the codes for the make it simple and get those in the middle of bar
## then done
### Data Wrangling
df <- read_csv("D:/VIS/data_makeovermonday/makeovermonday-2025w39-family-spending-in-the-uk/makeovermonday-2025w39-family-spending-in-the-uk/data/figure_5_poorer_households_spent_proportionally_more_on_housing_fuel_and_power_than_richer_households_in_fye_2024.csv")
df |> 
  pivot_longer(!of_total_weekly_expendtiure, 
               names_to = 'rank', values_to = 'percent') |>
  filter(!rank %in% c('all_households', '2nd', '3rd', '4th')) |>
  mutate(percent = ifelse(rank == "bottom_fifth", -percent, percent), 
         of_total_weekly_expendtiure = fct_reorder(of_total_weekly_expendtiure, 
                                                   -percent,.fun = max)) |> 
  ggplot(aes(of_total_weekly_expendtiure, percent, fill = rank)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c('top_fifth' = "#E5AD4FFF", "bottom_fifth" = "#BD5630FF"), 
                    labels = c('top_fifth' = "Top", "bottom_fifth" = "Bottom")) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
  geom_text(aes(x = 'Housing (net) fuel and power', label = 'Housing', y = 0), 
            size = 7, color = 'white', family = 'robo', hjust =.5)+
  geom_text(aes(x = 'Food and non-alcoholic drinks', label = 'Food', y = 0), 
            size = 7, color = 'white', family = 'robo', hjust =.5)+
  geom_text(aes(x = 'Transport', label = 'Transport', y = 0), 
            size = 7, color = 'white', family = 'robo', hjust =.5)+
  geom_text(aes(x = 'Other expenditure items', label = 'Other', y = 0), 
            size = 7, color = 'white', family = 'robo', hjust =.5)+
  geom_text(aes(x = 'Recreation and culture', label = 'Recreation', y = 0), 
            size = 7, color = 'white', family = 'robo', hjust =.5)+
  geom_text(aes(x = 'Miscellaneous goods and services', label = 'Misc. Goods', y = 0), 
            size = 7, color = 'white', family = 'robo', hjust =.5)+
  geom_text(aes(x = 'Household goods and services', label = 'Household', y = 0), 
            size = 7, color = 'white', family = 'robo', hjust =.5)+
  geom_text(aes(x = 'Restaurants and hotels', label = 'Rest & Hotels', y = 0), 
            size = 7, color = 'white', family = 'robo', hjust =.5)+
  geom_text(aes(x = 'Communication', label = 'Communication', y = 0), 
            size = 7, color = 'white', family = 'robo', hjust =.5)+
  geom_text(aes(x = 'Clothing and footwear', label = 'Clothing', y = 0), 
            size = 7, color = 'white', family = 'robo', hjust =.5)+
  geom_text(aes(x = 'Alcoholic drink tobacco and narcotics', label = 'Alcohol', y = 0), 
            size = 7, color = 'white', family = 'robo', hjust =.5)+
  labs(x = '', 
       y = '', 
       title = "Family Spending in the UK: Comparing Top and Bottom Income Groups", 
       caption = 'Data: ONS | Vis: MhKirmizi', 
       fill = 'Income Group') +
  cowplot::theme_minimal_vgrid()+
  theme(legend.position = c(.10, .5),
        legend.title.align = .5,
        text = element_text(family = 'robo'),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 32, hjust = 1, 
                                  family = 'mons', 
                                  face = 'bold',
                                  color = "#121510FF"), 
        plot.caption = element_text(size = 14), 
        plot.background = element_rect(fill = 'white', color = 'white'), 
        panel.background = element_rect(fill = 'white', color = 'white'), 
        margin(t = 10, b=10, r= 10, l = 10)
        )
ggsave("household_spending.png", width = 1920, height = 1080, units = "px", dpi = 132)

 