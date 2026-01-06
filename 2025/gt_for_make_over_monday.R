library(tidyverse)
library(gt)
library(gtExtras)
### Colors
color_palette <- c("#CC79A7", "#009E73")
### time for gt
US_New_Year_Resolutions |> colnames()
tbl <- US_New_Year_Resolutions |> 
  mutate(
    across(2:6, ~ parse_number(.x) / 100)
  ) |> 
  gt() |> 
  opt_table_font(
    font = list(
      google_font("Lora"),
      default_fonts()
    )
  ) |> 
  cols_label(
    "Which New Year's Resolutions" = 'Resolutions', 
    "U.S. adult citizens" = 'Adult', 
    "18- to 29-year-olds"  = '18-29', 
    "30- to 44-year-olds" = '30-44', 
    "45- to 64-year-olds" = '45-65', 
    "65 and older" = '65+'
  ) |> 
  tab_spanner(
    label = md('Age Group'),
    columns = 3:6
  ) |> 
  tab_header(
    title = "New Year Resolution in US"
  ) |> 
  tab_footnote(
    footnote = 'Data: MakeoverMonday Week 52, 2025 | Desing: MhKirmizi', 
    placement = "auto"
  ) |> 
  tab_style(
    style = cell_text(
      font = google_font("Autour One"), 
      weight = "bold", 
      align = "left",
      size = px(40)
    ),
    locations = cells_title("title")
  ) |> 
  tab_style(
    style = cell_text(font = google_font("Autour One")), 
    locations = list(
      cells_column_labels(),
      cells_column_spanners()
    )
  ) |>
  data_color(columns = 2:6, 
             fn = scales::col_numeric(
               palette = c("white", "orange", "red"), 
               domain = 0:1
             )) |> 
  fmt_percent(columns = 2:6, 
             decimals = 0) |> 
  opt_stylize(style = 6, color = 'gray') |> 
  tab_options(
    data_row.padding = px(3)
  )
gtsave(tbl, 'week52.html')
