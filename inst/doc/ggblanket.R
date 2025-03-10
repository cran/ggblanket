## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 6,
  fig.asp = 0.6,
  out.width = "70%",
  dpi = 300
)

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

## -----------------------------------------------------------------------------
penguins2 |>
  gg_violin(
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
    mapping = aes(shape = sex),
  ) +
  guides(shape = guide_legend(override.aes = list(colour = grey)))

## -----------------------------------------------------------------------------
penguins2 |>
  gg_jitter(
    x = flipper_length_mm,
    y = body_mass_g,
    col = bill_length_mm,
    x_breaks_n = 4,
    x_label = "Flipper length",
    x_labels = \(x) paste0(x, " mm"),
    y_expand_limits = 1000,
    y_labels = label_number(big.mark = " "), 
    y_transform = "sqrt",
    col_label = "Bill\nlength (mm)",
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
    linewidth = 1, 
    linetype = "dashed",
    level = 0.999, 
    se = TRUE,
    blend = "multiply",
  ) 

## ----fig.asp=0.65-------------------------------------------------------------
penguins2 |>
  gg_histogram(
    x = flipper_length_mm,
    col = species,
    title = "Penguin flipper length by species",
    subtitle = "Palmer Archipelago, Antarctica",
    caption = "Source: Gorman, 2020", 
    theme = dark_mode_t() + theme(legend.title = element_blank()),
  ) 

## -----------------------------------------------------------------------------
p1 <- penguins2 |>
  gg_jitter(
    x = sex,
    y = bill_depth_mm,
  )

p2 <- penguins2 |>
  gg_jitter(
    x = bill_depth_mm,
    y = sex,
  ) 

p1 + p2

## -----------------------------------------------------------------------------
penguins2 |> 
  gg_violin(
    x = species, 
    y = bill_depth_mm,
    outliers = FALSE,
  ) +
  geom_boxplot(
    width = 0.25,
    colour = lightness[1],
    fill = lightness[2],
  ) +
  geom_jitter(
    colour = navy,
  ) 

## -----------------------------------------------------------------------------
penguins2 |>
  group_by(species, sex) |>
  summarise(
    lower = quantile(bill_depth_mm, probs = 0.05),
    upper = quantile(bill_depth_mm, probs = 0.95),
    bill_depth_mm = mean(bill_depth_mm, na.rm = TRUE),
  ) |>
  labelled::copy_labels_from(penguins2) |>
  gg_blanket(
    y = species,
    x = bill_depth_mm,
    xmin = lower, 
    xmax = upper,
    col = sex,
    position = position_dodge(),
    x_expand_limits = 0,
  ) +
  geom_col(
    width = 0.75,
    position = position_dodge(),
  ) +
  geom_errorbar(
    width = 0.1, 
    position = position_dodge(width = 0.75),
    colour = lightness[1],
  ) 

## -----------------------------------------------------------------------------
set_blanket(
  theme = rlang::list2(
    light_mode_t(
      base_size = 9,
      axis_line_colour = "#78909C",
      panel_grid_colour = "#C8D7DF",
      panel_background_fill = "#E8EFF2",
      axis_line_linewidth = 0.25, 
      panel_grid_linewidth = 0.25, 
    ),
    labs(colour = NULL, fill = NULL),
  ),
  colour = "tan", 
  col_palette_d = c("#003f5c", "#bc5090", "#ffa600", "#357BA2"),
  col_palette_na_d = "#78909C",
)

p1 <- penguins2 |>
  gg_point(
    x = bill_depth_mm, 
    y = flipper_length_mm,
  ) 

p2 <- penguins2 |> 
  gg_bar(
    y = sex,
    col = species,
    position = "dodge",
  ) 

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

