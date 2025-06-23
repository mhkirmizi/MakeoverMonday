library(tidyverse)
library(showtext)
### Fonts
font_add_google("Cinzel", "cinzel")
showtext.auto()
### Data Wrangling & Plotting
df |> 
  group_by(house, gender) |> 
  count()
unique(df$loyalty)
unique(df$house)
df2 <- df |> 
  mutate(loyalty = case_when(
    grepl("Lord Voldemort|Death Eaters", loyalty, ignore.case = TRUE) ~ "Lord Voldemort",
    grepl("Dumbledore", loyalty, ignore.case = TRUE) ~ "Harry Potter",
    grepl("Order of The Phoenix", loyalty, ignore.case = TRUE) ~ "Harry Potter",
    grepl("Minister", loyalty, ignore.case = TRUE) ~ "Minister of Magic",
    TRUE ~ "Not Aligned"
  ))
df2 |> 
  group_by(loyalty) |> 
  count() |> 
  ggplot(aes(x = reorder(loyalty, n), y = n, fill = loyalty)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Number of Students", 
       y = "", 
       caption = "Data: Kaggle(via MakeoverMonday) | Vis: MhKirmizi", 
       title = "LOYALTY of CHARACTERS in HARRY POTTER",
       fill = "Loyalty") +
  theme_minimal(base_size = 12, base_family = "cinzel") +
  theme(legend.position = "top", 
        plot.title = element_text(size = 36, family = "cinzel", face = "bold", hjust = .5), 
        plot.background = element_rect(fill = "white", color = "white"), 
        panel.background = element_rect(fill = "white", color = "white"))
ggsave("harryPotter.png", width = 1920, height = 1080, units = "px", dpi = 132)
