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
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(ggblanket)
library(palmerpenguins)
library(patchwork)

penguins2 <- penguins |> 
  labelled::set_variable_labels(
    bill_length_mm = "Bill length (mm)",
    bill_depth_mm = "Bill depth (mm)",
    flipper_length_mm = "Flipper length (mm)",
    body_mass_g = "Body mass (g)",
  ) |> 
  mutate(sex = factor(sex, labels = c("Female", "Male"))) |> 
  tidyr::drop_na(sex) 

## -----------------------------------------------------------------------------
set_blanket()

## -----------------------------------------------------------------------------
penguins2 |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
  )

## ----fig.asp=0.45-------------------------------------------------------------
penguins2 |>
  gg_boxplot(
    x = flipper_length_mm,
    y = island,
    col = sex, 
  )

## -----------------------------------------------------------------------------
penguins2 |>
  gg_histogram(
    x = flipper_length_mm,
    facet = species,
  )

## ----fig.asp=0.75-------------------------------------------------------------
penguins2 |>
  gg_histogram(
    x = flipper_length_mm,
    facet = species,
    facet2 = sex,
  )

## -----------------------------------------------------------------------------
penguins2 |> 
  gg_jitter(
    x = species, 
    y = flipper_length_mm, 
    col = island,
    mapping = aes(shape = island),
  ) 

## -----------------------------------------------------------------------------
penguins2 |>
  gg_jitter(
    x = flipper_length_mm,
    y = body_mass_g,
    col = flipper_length_mm,
    x_breaks = scales::breaks_extended(n = 4, only.loose = TRUE),
    x_labels = \(x) stringr::str_sub(x, 1, 1),
    y_expand_limits = 1000,
    y_labels = label_number(big.mark = " "), 
    y_transform = "log10",
    col_label = "Flipper\nlength (mm)",
    col_steps = TRUE,
    col_breaks = \(x) quantile(x, seq(0, 1, 0.25)),
    col_palette = viridis::rocket(n = 9, direction = -1),
  )

## ----fig.asp=0.6--------------------------------------------------------------
penguins2 |>
  gg_freqpoly(
    x = flipper_length_mm,
    col = species,
  )

## -----------------------------------------------------------------------------
penguins2 |>
  gg_smooth(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex, 
    col_palette = c("#003f5c", "#ffa600"),
    colour = "#bc5090", 
    linewidth = 1, 
    linetype = "dashed",
    alpha = 1, 
    se = TRUE, 
    level = 0.999, 
  ) 

## ----fig.asp=0.65-------------------------------------------------------------
penguins2 |>
  gg_histogram(
    x = flipper_length_mm,
    col = species,
    title = "Penguin flipper length by species",
    subtitle = "Palmer Archipelago, Antarctica",
    caption = "Source: Gorman, 2020", 
    mode = grey_mode_t() + theme(legend.title = element_blank()),
  ) 

## -----------------------------------------------------------------------------
p1 <- penguins2 |>
  gg_jitter(
    x = sex,
    y = bill_depth_mm,
    subtitle = "\nx_orientation = TRUE",
  )

p2 <- penguins2 |>
  gg_jitter(
    x = bill_depth_mm,
    y = sex,
    subtitle = "\ny_orientation = TRUE",
  ) 

p1 + p2

## ----fig.asp = 0.5------------------------------------------------------------
penguins2 |>
  group_by(species, sex) |>
  summarise(
    lower = quantile(flipper_length_mm, probs = 0.05),
    upper = quantile(flipper_length_mm, probs = 0.95),
    flipper_length_mm = mean(flipper_length_mm, na.rm = TRUE),
  ) |>
  labelled::copy_labels_from(penguins2) |>
  gg_col(
    x = flipper_length_mm,
    xmin = lower, 
    xmax = upper,
    y = species,
    col = sex,
    position = position_dodge(),
    width = 0.75,
  ) +
  geom_errorbar(
    width = 0.1, 
    position = position_dodge(width = 0.75),
    colour = lightness[1],
  )  

## -----------------------------------------------------------------------------
set_blanket(
  mode = dark_mode_r(), 
  geom_colour = "#e7298a",
  annotate_colour = darkness[1],
  col_palette_d = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", 
                    "#e6ab02", "#a6761d", "#666666"), #RColorBrewer Dark2 
)

p1 <- penguins2 |>
  gg_point(
    x = flipper_length_mm, 
    y = body_mass_g,
    x_breaks = breaks_extended(n = 4, only.loose = TRUE),
  ) +
  geom_vline(xintercept = 200) +
  annotate("text", x = I(0.25), y = I(0.75), label = "Here")

p2 <- penguins2 |> 
  gg_histogram(
    x = flipper_length_mm,
    col = species,
    x_breaks = breaks_extended(n = 4, only.loose = TRUE),
  ) +
  geom_vline(xintercept = 200) +
  annotate("text", x = I(0.75), y = I(0.75), label = "Here")

p1 + p2

set_blanket()

## -----------------------------------------------------------------------------
geom_spoke()

expand.grid(x = 1:10, y = 1:10) |>
  tibble() |>
  mutate(angle = runif(100, 0, 2*pi)) |>
  mutate(speed = runif(100, 0, sqrt(0.1 * x))) |>
  gg_blanket(
    geom = "spoke",
    x = x, 
    y = y,
    col = speed,
    mapping = aes(angle = angle, radius = speed),
  ) +
  geom_point()

