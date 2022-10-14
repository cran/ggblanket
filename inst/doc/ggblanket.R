## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 6,
  fig.asp = 0.618,
  out.width = "70%",
  dpi = 300
)

## ----setup--------------------------------------------------------------------
library(ggblanket)
library(ggplot2)
library(dplyr)
library(stringr)
library(palmerpenguins)

## -----------------------------------------------------------------------------
iris |>
  mutate(Species = str_to_sentence(Species)) |> 
  gg_point(
    x = Sepal.Width, 
    y = Sepal.Length, 
    col = Species)

## -----------------------------------------------------------------------------
penguins |> 
  gg_histogram(
    x = body_mass_g, 
    col = species) 

## -----------------------------------------------------------------------------
penguins |> 
  tidyr::drop_na(sex) |>
  mutate(sex = str_to_sentence(sex)) |>
  gg_violin(
    x = sex, 
    y = body_mass_g, 
    facet = species)

## ---- fig.asp=0.75------------------------------------------------------------
penguins |>
  tidyr::drop_na(sex) |>
  mutate(sex = str_to_sentence(sex)) |> 
  gg_density(
    x = flipper_length_mm,
    col = sex,
    facet = species,
    facet2 = island
  )

## -----------------------------------------------------------------------------
penguins |> 
  mutate(sex = str_to_sentence(sex)) |> 
  group_by(species, sex) |> 
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE)) |> 
  gg_col(
    x = species, 
    y = body_mass_g, 
    col = sex, 
    position = position_dodge2(preserve = "single"),
    pal = c("#1B9E77", "#9E361B"))

## -----------------------------------------------------------------------------
penguins |>
  gg_jitter(
    x = species,
    y = body_mass_g,
    col = flipper_length_mm,
    col_continuous = "steps",
    y_include = 0,
    y_trans = "sqrt",
    y_breaks = scales::breaks_width(1500), 
    y_labels = scales::label_number()
  )

## -----------------------------------------------------------------------------
penguins |>
  group_by(species, sex) |>
  summarise(across(flipper_length_mm, ~ round(mean(.x, na.rm = TRUE)), 0)) |> 
  gg_tile(
    x = sex, 
    y = species, 
    col = flipper_length_mm, 
    width = 0.9,
    height = 0.9,
    pal = rev(pals::brewer.rdbu(9)), 
    col_legend_place = "r",
    col_rescale = c(186, 215, 222),
    x_labels = snakecase::to_sentence_case,
    title = "Average penguin body mass",
    subtitle = "Palmer Archipelago, Antarctica",
    theme = gg_theme(plot_background_pal = "white",
                     axis_line_pal = "white", 
                     axis_ticks_pal = "white")) +
  geom_text(aes(label = flipper_length_mm), col = "#232323", size = 3.5) 

## -----------------------------------------------------------------------------
storms |>
  group_by(year) |>
  filter(between(year, 1980, 2020)) |>
  summarise(wind = mean(wind, na.rm = TRUE)) |>
  gg_line(
    x = year,
    y = wind,
    x_labels = scales::label_number(big.mark = ""),
    y_include = 0,
    title = "Storm wind speed",
    subtitle = "USA average storm wind speed, 1980\u20132020",
    y_title = "Wind speed (knots)",
    caption = "Source: NOAA"
  ) +
  geom_point()

## -----------------------------------------------------------------------------
penguins |>
  tidyr::drop_na(sex) |> 
  group_by(species, sex, island) |>
  summarise(body_mass_kg = mean(body_mass_g) / 1000) |>
  gg_col(
    x = body_mass_kg, 
    y = species, 
    col = sex, 
    facet = island,
    width = 0.75,
    col_labels = snakecase::to_sentence_case, 
    position = "dodge")

## -----------------------------------------------------------------------------
penguins |>
  gg_boxplot(x = species,
             y = body_mass_g,
             width = 0.5,
             pal = "#1B9E77", 
             outlier.colour = NA) +
  geom_jitter()

## -----------------------------------------------------------------------------
penguins |>
  tidyr::drop_na(sex) |>
  gg_smooth(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    size = 0.5, 
    level = 0.99, 
    col_legend_place = "t",
    col_title = "", 
    col_labels = snakecase::to_sentence_case
  ) 

## -----------------------------------------------------------------------------
penguins |>
  mutate(sex = str_to_sentence(sex)) |> 
  gg_point(x = bill_depth_mm,
           y = bill_length_mm,
           col = sex,
           facet = species, 
           pal = c("#1B9E77", "#9E361B"), 
           theme = theme_grey())

## -----------------------------------------------------------------------------
storms |>
  group_by(year) |>
  filter(between(year, 1980, 2020)) |>
  summarise(wind = mean(wind, na.rm = TRUE)) |>
  gg_col(
    x = year,
    y = wind,
    x_labels = scales::label_comma(big.mark = ""),
    x_expand = c(0, 0),
    width = 0.75,
    theme = gg_theme(
      text_size = 11,
      plot_background_pal = "white",
      panel_background_pal = "white"))

## -----------------------------------------------------------------------------
gg_point_custom <- function(data, x, y, col, 
                            size = 3, 
                            shape = 17,
                            pal = pals::brewer.dark2(9), 
                            col_title = "", 
                            col_legend_place = "t",
                            ...) {
  data |> 
    gg_point(x = {{ x }}, y = {{ y }}, col = {{col}}, 
             size = size, 
             shape = shape,
             pal = pal, 
             col_title = col_title, 
             col_legend_place = col_legend_place, 
             ...)
}

iris |>
  mutate(Species = str_to_sentence(Species)) |> 
  gg_point_custom(
    x = Sepal.Width,
    y = Sepal.Length,
    col = Species, 
    title = "Edgar Anderson's iris data",
    subtitle = "Iris sepal length by width and species",
    caption = "Edgar Anderson, 1935"
  )

## ---- eval = F----------------------------------------------------------------
#  theme_custom <- gg_theme(
#    "helvetica",
#    plot_background_pal = "white",
#    panel_background_pal = "white",
#  )
#  
#  iris |>
#    mutate(Species = str_to_sentence(Species)) |>
#    add_tooltip_text(titles = snakecase::to_sentence_case) |>
#    gg_point(
#      x = Sepal.Width,
#      y = Sepal.Length,
#      col = Species,
#      text = text,
#      col_legend_place = "r",
#      theme = theme_custom) |>
#    plotly::ggplotly(tooltip = "text")

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics("../man/figures/ggplotly_screenshot.png", dpi = 300)

## -----------------------------------------------------------------------------
df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

df %>% 
  gg_col(
    x = trt,
    y = resp,
    col = group,
    position = "dodge",
    y_include = max(df$upper) 
  ) +
  geom_errorbar(
    aes(x = trt, ymin = lower, ymax = upper, group = group),
    inherit.aes = FALSE,
    col = "#7F7F7F",
    position = position_dodge(width = 0.9),
    width = 0.1
    )

## -----------------------------------------------------------------------------
penguins |> 
  gg_blank(x = flipper_length_mm, y = body_mass_g, col = species) +
  geom_point(aes(shape = species)) +
  labs(shape = "Species")

## -----------------------------------------------------------------------------
penguins %>% 
  gg_point(
    x = flipper_length_mm, 
    y = body_mass_g, 
    coord = coord_cartesian(xlim = c(190, 200)),
    x_breaks = scales::breaks_width(2))

## ---- fig.asp=0.75------------------------------------------------------------
penguins |>
  tidyr::drop_na() |>
  mutate(sex = str_to_sentence(sex)) |>
  gg_jitter(
    x = sex, 
    y = body_mass_g,
    col = sex, 
    size = 1, 
    x_title = "",
    y_limits = c(NA, NA)) +
  facet_grid(cols = vars(species),
             rows = vars(island), 
             switch = "y")

## -----------------------------------------------------------------------------
diamonds |> 
  ggplot() +
  geom_density2d(aes(x = carat, y = price, colour = after_stat(level))) +
  scale_colour_gradientn(colours = viridis::viridis(9)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6000)) +
  labs(x = "Carat", y = "Price", colour = "Level") +
  gg_theme() +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.position = "right") +
  theme(legend.direction = "vertical")

