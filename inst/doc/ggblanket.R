## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 6,
  fig.asp = 0.6,
  out.width = "70%",
  dpi = 300)

## ----setup--------------------------------------------------------------------
library(ggblanket)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(palmerpenguins)
library(patchwork)

## -----------------------------------------------------------------------------
penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
  )

## -----------------------------------------------------------------------------
penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g, 
    col = species,
  )

## ----fig.asp=0.45-------------------------------------------------------------
penguins |>
  drop_na(sex) |>
  mutate(across(sex, str_to_sentence)) |>
  gg_bar(
    position = "dodge",
    y = species, 
    col = sex,
    width = 0.75,
  )

## -----------------------------------------------------------------------------
penguins |>
  drop_na(sex) |> 
  mutate(across(sex, str_to_sentence)) |> 
  gg_histogram(
    x = flipper_length_mm,
    facet = species,
  )

## ----fig.asp=0.75-------------------------------------------------------------
penguins |>
  mutate(across(sex, str_to_sentence)) |> 
  gg_histogram(
    x = flipper_length_mm,
    facet = species,
    facet2 = sex,
  )

## -----------------------------------------------------------------------------
penguins |>
  gg_jitter(
    x = species, 
    y = body_mass_g, 
    col_pal = "#7FCDBB",
  )

## ----fig.asp=0.33-------------------------------------------------------------
viridisLite::rocket(n = 9)

sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) |> 
  gg_sf(
    col = AREA,
    col_pal = viridisLite::rocket(n = 9)
  )

## -----------------------------------------------------------------------------
penguins |>
  drop_na(sex) |>  
  gg_jitter(
    x = species,
    y = body_mass_g,
    col = flipper_length_mm,
    facet = sex,
    x_labels = \(x) str_sub(x, 1, 1),
    y_breaks = scales::breaks_width(1000),
    y_expand_limits = 2000,
    y_labels = scales::label_number(big.mark = " "), 
    y_transform = "log10",
    y_title = "Body mass (g)",
    col_steps = TRUE,
    col_breaks = \(x) quantile(x, seq(0, 1, 0.25)),
    facet_labels = str_to_sentence,
  )

## ----echo = FALSE,   fig.width = 3, fig.asp = 2-------------------------------
knitr::include_graphics("autocomplete_y.png", dpi = 300)

## ----fig.asp=0.6--------------------------------------------------------------
diamonds |>
  gg_hex(
    coord = coord_cartesian(clip = "on"), 
    x = carat,
    y = price,
    y_limits = c(0, 20000),
  )

## ----echo=FALSE---------------------------------------------------------------
d <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

p1 <- d |>
  gg_errorbar(
    x = trt,
    ymin = lower,
    ymax = upper,
    col = group,
    width = 0.1,
    x_title = "Treatment",
    y_title = "Response",
    subtitle = "\nmode = light_mode_n(),",
    mode = light_mode_n(),
  ) 

p2 <- d |>
  gg_errorbar(
    x = trt,
    ymin = lower,
    ymax = upper,
    col = group,
    width = 0.1,
    x_title = "Treatment",
    y_title = "Response",
    subtitle = "\n+ light_mode_n()"
  ) +
  light_mode_n() 

p1 + p2 

## ----fig.asp=0.7--------------------------------------------------------------
penguins |>
  gg_histogram(
    x = flipper_length_mm,
    col = species,
    title = "Penguin flipper length by species",
    subtitle = "Palmer Archipelago, Antarctica",
    caption = "Source: Gorman, 2020", 
    mode = light_mode_t(),
  ) +
  labs(colour = NULL, fill = NULL)

## ----fig.asp=0.75-------------------------------------------------------------
penguins |>
  gg_histogram(
    x = flipper_length_mm,
    col = species,
    title = "Penguin flipper length by species",
    subtitle = "Palmer Archipelago, Antarctica",
    caption = "Source: Gorman, 2020", 
    mode = grey_mode_b(),
  ) 

## ----fig.asp=0.65-------------------------------------------------------------
penguins |>
  gg_histogram(
    x = flipper_length_mm,
    col = species,
    col_pal = c(teal, orange, plum),
    title = "Penguin flipper length by species",
    subtitle = "Palmer Archipelago, Antarctica",
    caption = "Source: Gorman, 2020", 
    mode = dark_mode_r(),
  ) 

## ----echo=FALSE---------------------------------------------------------------
d <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

p1 <- d |>
  gg_errorbar(
    x = trt,
    ymin = lower,
    ymax = upper,
    col = group,
    width = 0.1,
    x_title = "Treatment",
    y_title = "Response",
    subtitle = "\nDefault y scale",
    mode = light_mode_n(),
  ) 

p2 <- d |>
  gg_errorbar(
    x = trt,
    ymin = lower,
    ymax = upper,
    col = group,
    width = 0.1,
    x_title = "Treatment",
    y_title = "Response",
    y_limits = c(NA, NA),
    subtitle = "\ny_limits = c(NA, NA),",
    mode = light_mode_n(),
  ) 

p3 <- d |>
  gg_col(
    position = "dodge",
    x = trt,
    y = upper,
    col = group,
    width = 0.5,
    x_title = "Treatment upper",
    y_title = "Response",
    y_limits = c(0, NA),
    subtitle = "\ny_limits = c(0, NA),",
    mode = light_mode_n(),
  ) 

p1 + p2 + p3

## -----------------------------------------------------------------------------
penguins |>
  mutate(across(sex, str_to_sentence)) |> 
  drop_na(sex) |> 
  gg_smooth(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex, 
    se = TRUE, # via ... from geom_smooth
    level = 0.999, # via ... from geom_smooth
  ) 

## ----fig.asp=0.4--------------------------------------------------------------
penguins |>
  gg_boxplot(
    position = position_dodge2(preserve = "single"),
    x = flipper_length_mm,
    y = species,
    alpha_pal = 0, #or col_pal = scales::alpha(blue, 0),
  )

## -----------------------------------------------------------------------------
penguins |>
  gg_boxplot(
    position = position_dodge2(preserve = "single"),
    x = species,
    y = flipper_length_mm,
    col = sex,
    alpha_pal = 0,
  )

## ----fig.asp = 0.4------------------------------------------------------------
penguins |>
  group_by(species) |>
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE)) |>
  mutate(lower = body_mass_g * 0.95) |> 
  mutate(upper = body_mass_g * 1.2) %>%
  gg_col(
    x = body_mass_g,
    xmin = lower, 
    xmax = upper,
    y = species,
    col = species,
    width = 0.75,
    x_expand_limits = c(0, max(.$upper)),
    x_labels = \(x) x / 1000, 
    x_title = "Body mass kg", 
  ) +
  geom_errorbar(
    colour = "black", 
    width = 0.1, 
  ) 

## ----fig.asp=0.4--------------------------------------------------------------
penguins |>
  group_by(species) |>
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE)) |>
  mutate(lower = body_mass_g * 0.95) |> 
  mutate(upper = body_mass_g * 1.2) |> 
  gg_blanket( 
    x = body_mass_g,
    y = species,
    col = species,
    xmin = lower, 
    xmax = upper,
    width = 0.75,
    x_expand_limits = 0,
    x_labels = \(x) x / 1000, 
    x_title = "Body mass kg",
  ) +
  geom_col(
    colour = "#d3d3d3",
    fill = "#d3d3d3",
    alpha = 0.9,
    width = 0.75,
  ) +
  geom_errorbar(
    width = 0.1, 
  ) 

## -----------------------------------------------------------------------------
penguins |> 
  gg_point(
    x = flipper_length_mm, 
    y = body_mass_g, 
    col = species,
    alpha = species,
    alpha_pal = c(1, 1, 0.33),
    mapping = aes(shape = species),
  ) 

## -----------------------------------------------------------------------------
penguins |>
  gg_blanket(
    geom = "bar",
    stat = "bin",
    position = "stack",
    x = flipper_length_mm, 
    col = species,
  ) 

## -----------------------------------------------------------------------------
penguins |>
  drop_na(sex) |>
  mutate(across(sex, str_to_sentence)) |>
  gg_blanket(
    geom = "violin",
    stat = "ydensity",
    position = "dodge",
    x = sex,
    y = body_mass_g,
    col = species,
  )

