library(tidyverse)
library(showtext)
library(ggpubr)

font_add_google('Autour One')
font_add_google('Merriweather Sans', 'merr')
showtext_auto()

### Plotting
df |> 
  rename(Height = `Drop (ft)`) |> 
  ggbarplot(x = 'Name',
            y = 'Height', 
            fill = "#076fa2", 
            sort.val = 'asc', 
            palette =  "Accent",
            ylab = NULL, 
            rotate = TRUE
            ) +
  geom_text(aes(y = 150, x = Name, label = Country), size = 5, color = 'white') +
  labs(title = 'The World’s Tallest Roller Coasters, by Drop Height', 
       caption = 'Data: MakeoverMonday 2026-W1 | Vis: MhKirmizi') +
  ggimprensa::tema_nexo(base_size = 14, base_family = 'Autour One') +
  theme(
    plot.title = element_text(size = 28, 
                              family = 'merr', face = 'bold', hjust = .5),
    legend.position = 'none',
    plot.caption = element_text(hjust = 1))
ggsave("week1.png", width = 1920, height = 1080, units = "px", dpi = 132)

