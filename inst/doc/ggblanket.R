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
library(patchwork)

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
    facet2 = island)

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
    y_labels = scales::label_number())

## -----------------------------------------------------------------------------
penguins |>
  group_by(species, sex) |>
  summarise(
    flipper_length_mm = round(mean(flipper_length_mm, na.rm = TRUE), 0)) |>
  gg_tile(
    x = sex, 
    y = species, 
    col = flipper_length_mm, 
    width = 0.9,
    height = 0.9,
    pal = rev(pals::brewer.rdbu(9)), 
    col_legend_place = "r",
    col_rescale = c(186, 215, 222),
    x_labels = stringr::str_to_sentence,
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
    caption = "Source: NOAA") +
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
    col_labels = stringr::str_to_sentence, 
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
    linewidth = 0.5, #accessed via geom_smooth
    level = 0.99, #accessed via geom_smooth
    col_legend_place = "t",
    col_title = "", 
    col_labels = stringr::str_to_sentence, 
    colour = "white") #accessed via geom_smooth

## -----------------------------------------------------------------------------
penguins |>
  mutate(sex = str_to_sentence(sex)) |> 
  gg_point(x = flipper_length_mm,
           y = body_mass_g,
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
    theme = gg_theme(
      text_size = 11,
      plot_background_pal = "white",
      panel_background_pal = "white"))

## -----------------------------------------------------------------------------
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

nc |>
  gg_sf(col = AREA,
        pal = pals::brewer.reds(9),
        title = "**Bold** or _italics_ or <span style = 'color:red;'>red</span>",
        theme = gg_theme(title_face = "plain")) +
  theme(plot.title = ggtext::element_markdown())

## -----------------------------------------------------------------------------
p1 <- diamonds |> 
  gg_point(
    x = carat,
    y = price)

p2 <- diamonds |> 
  gg_bin2d(
    x = carat,
    y = price,
    caption = "Source: Diamonds Association",
    theme = gg_theme(caption_hjust = 1))

p1 + p2

## -----------------------------------------------------------------------------
p1 <- penguins |>
  gg_blank(
    x = flipper_length_mm,
    y = body_mass_g,
    col = species,
    pal = rep(pal_viridis_mix(1), 3), 
    x_breaks = scales::breaks_pretty(3),
    col_legend_ncol = 2) +
  geom_point(aes(alpha = species)) +
  labs(alpha = "Species") +
  scale_alpha_manual(values = c(0.1, 1, 0.5))

p2 <- penguins |>
  gg_blank(
    x = flipper_length_mm, 
    y = body_mass_g, 
    col = species, 
    x_breaks = scales::breaks_pretty(3), 
    col_legend_ncol = 2) +
  geom_point(aes(alpha = species)) +
  labs(alpha = "Species") +
  scale_alpha_manual(values = c(0.1, 1, 0.5))

p1 + p2

## ---- eval = FALSE------------------------------------------------------------
#  df <- iris |>
#    tibble::tibble() |>
#    add_tooltip(Sepal.Width, Sepal.Length, Species)
#  
#  df |>
#    slice_head()
#  
#  b <- iris |>
#    add_tooltip(tidyselect::contains("Sepal"), Species) |>
#    mutate(id = row_number()) |>
#    gg_blank(x = Sepal.Width,
#             y = Sepal.Length,
#             col = Species,
#             facet = Species,
#             theme = gg_theme("helvetica"),
#             facet_labels = stringr::str_to_sentence)
#  
#   p <- b +
#    ggiraph::geom_point_interactive(aes(tooltip = tooltip, data_id = id))
#  
#  ggiraph::girafe(
#    ggobj = p,
#    width_svg = 5,
#    height_svg = 3,
#    options = list(
#      ggiraph::opts_tooltip(use_fill = TRUE, use_stroke = TRUE),
#      ggiraph::opts_hover(css = "fill: #d62728; stroke: #d62728"))
#  )

## ---- echo = FALSE------------------------------------------------------------
knitr::include_graphics("../man/figures/ggiraph_screenshot.png", dpi = 300)

## ---- eval = FALSE------------------------------------------------------------
#  b +
#    geom_point(aes(text = tooltip)) |>
#    plotly::ggplotly(tooltip = "text")

## ---- fig.asp=0.7-------------------------------------------------------------
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
    caption = "Edgar Anderson, 1935")

## -----------------------------------------------------------------------------
df <- data.frame(
  treatment = factor(c(1, 1, 2, 2)),
  response = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

df %>%  
  gg_col(
    x = response,
    y = treatment,
    col = group,
    position = "dodge",
    x_include = max(.$upper),
    width = 0.66, 
    pal = c("#1B9E77", "#9E361B")) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper, group = group),
    col = "#7F7F7F",
    position = position_dodge(width = 0.66),
    width = 0.1)

## -----------------------------------------------------------------------------
iris |>
  mutate(Species = stringr::str_to_sentence(Species)) |> 
  gg_blank(
    x = Sepal.Width,
    y = Sepal.Length,
    col = Species,
    facet = Species,
    col_legend_place = "r",
    col_labels = stringr::str_to_sentence) +
  ggdensity::geom_hdr(colour = NA) +
  labs(alpha = "Probs") +
  theme(legend.title = element_text(margin = margin(t = 5)))

