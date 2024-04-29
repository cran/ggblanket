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

set_blanket()

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
    col_palette = viridis::magma(n = 9, direction = -1),
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

## -----------------------------------------------------------------------------
penguins |>
  mutate(across(sex, str_to_sentence)) |> 
  drop_na(sex) |> 
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

## ----fig.asp=0.5--------------------------------------------------------------
penguins |> 
  gg_boxplot(
    y = island, 
    x = flipper_length_mm, 
    col = species,
    fill = NA,
    position = position_dodge2(preserve = "single")
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
    position = "dodge2",
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

## ----fig.asp = 0.4------------------------------------------------------------
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
  gg_jitter(
    y = species, 
    x = flipper_length_mm, 
    col = species,
    mapping = aes(shape = species),
  ) +
  guides(shape = guide_legend(reverse = TRUE)) +
  scale_shape_manual(values = rev(scales::shape_pal()(3)))

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
    title = "Penguin flipper length by species",
    subtitle = "Palmer Archipelago, Antarctica",
    caption = "Source: Gorman, 2020", 
    mode = dark_mode_r(),
  ) 

## -----------------------------------------------------------------------------
set_blanket(
  mode = grey_mode_r(), 
  geom_colour = "#ffa600",
)

p1 <- penguins |>
  gg_point(
    x = flipper_length_mm, 
    y = body_mass_g,
    x_breaks = scales::breaks_pretty(3),
  ) +
  geom_vline(xintercept = 200) +
  annotate("text", x = I(0.25), y = I(0.75), label = "Here")

p2 <- penguins |> 
  gg_histogram(
    x = flipper_length_mm,
    x_breaks = scales::breaks_pretty(3),
  ) +
  geom_vline(xintercept = 200) +
  annotate("text", x = I(0.75), y = I(0.75), label = "Here")

p1 + p2

## -----------------------------------------------------------------------------
set_blanket(
  mode = dark_mode_r(), 
  geom_colour = "#bc5090",
  annotate_colour = "#c8d7df",
)

p1 <- penguins |>
  gg_point(
    x = flipper_length_mm, 
    y = body_mass_g,
    x_breaks = scales::breaks_pretty(3),
  ) +
  geom_vline(xintercept = 200) +
  annotate("text", x = I(0.25), y = I(0.75), label = "Here")

p2 <- penguins |> 
  gg_histogram(
    x = flipper_length_mm,
    x_breaks = scales::breaks_pretty(3),
  ) +
  geom_vline(xintercept = 200) +
  annotate("text", x = I(0.75), y = I(0.75), label = "Here")

p1 + p2

## -----------------------------------------------------------------------------
set_blanket(
  light_mode_t() + theme(legend.title = element_blank())
)

geom_violin()

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

## -----------------------------------------------------------------------------
geom_histogram()

penguins |>
  gg_blanket(
    geom = "bar",
    stat = "bin",
    position = "stack",
    x = flipper_length_mm, 
    col = species,
  ) 

## -----------------------------------------------------------------------------
set_blanket()

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

