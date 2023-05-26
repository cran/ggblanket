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
library(dplyr)
library(ggplot2)
library(stringr)
library(ggblanket)
library(palmerpenguins)

penguins <- penguins |>
  mutate(sex = str_to_sentence(sex)) |>
  tidyr::drop_na(sex)

## -----------------------------------------------------------------------------
penguins |> 
  ggplot() + 
  geom_point(aes(x = flipper_length_mm, y = body_mass_g))

## -----------------------------------------------------------------------------
penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g)

## -----------------------------------------------------------------------------
penguins |> 
  ggplot() + 
  geom_point(aes(x = flipper_length_mm, y = body_mass_g,
                 col = species))

## -----------------------------------------------------------------------------
penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g, 
    col = species)

## -----------------------------------------------------------------------------
penguins |>
  ggplot() +
  geom_density(aes(x = body_mass_g, fill = species))

## -----------------------------------------------------------------------------
penguins |>
  gg_density(
    x = body_mass_g, 
    col = species)

## -----------------------------------------------------------------------------
penguins |>
  ggplot() +
  geom_histogram(
    aes(x = body_mass_g),
    fill = "#1B9E77")

## -----------------------------------------------------------------------------
penguins |>
  gg_histogram(
    x = body_mass_g, 
    pal = "#1B9E77")

## -----------------------------------------------------------------------------
penguins |>
  ggplot() +
  geom_jitter(aes(x = species, y = body_mass_g, col = sex)) +
  scale_color_manual(values = c("#1B9E77", "#9E361B"))

## -----------------------------------------------------------------------------
penguins |>
  gg_jitter(
    x = species, 
    y = body_mass_g, 
    col = sex, 
    pal = c("#2596be", "#fc7c24"))

## -----------------------------------------------------------------------------
penguins |>
  ggplot() +
  geom_violin(aes(x = sex, y = body_mass_g)) +
  facet_wrap(~species) 

## -----------------------------------------------------------------------------
penguins |>
  gg_violin(
    x = sex,
    y = body_mass_g,
    facet = species)

## ---- fig.asp=0.75------------------------------------------------------------
penguins |>
  ggplot() +
  geom_histogram(aes(x = flipper_length_mm)) +
  facet_grid(species ~ sex)

## -----------------------------------------------------------------------------
penguins |>
  gg_histogram(
    x = flipper_length_mm,
    facet = sex,
    facet2 = species)

## ---- echo = FALSE------------------------------------------------------------
knitr::include_graphics("screenshot_autotab_y.png", dpi = 300)

## -----------------------------------------------------------------------------
penguins |>
  ggplot() +
  geom_jitter(aes(x = species, y = body_mass_g, col = sex)) +
  expand_limits(y = 0) +
  scale_x_discrete(labels = \(x) str_sub(x, 1, 1)) +
  scale_y_continuous(breaks = scales::breaks_width(1500),
                     labels = scales::label_number(big.mark = " "),
                     trans = "sqrt") +
  labs(x = "Species", y = "Body mass (g)", col = "Sex") +
  theme(legend.position = "top") +
  theme(legend.justification = "left") +
  scale_colour_manual(values = scales::hue_pal()(2), 
                      guide = ggplot2::guide_legend(title.position = "top"))

## -----------------------------------------------------------------------------
penguins |>
  gg_jitter(
    x = species,
    y = body_mass_g,
    col = sex,
    x_labels = \(x) str_sub(x, 1, 1),
    y_include = 0,
    y_breaks = scales::breaks_width(1500), 
    y_labels = scales::label_number(big.mark = " "), 
    y_trans = "sqrt",
    y_title = "Body mass (g)", 
    col_legend_place = "t")

## ---- fig.asp = 0.618---------------------------------------------------------
library(patchwork)
p1 <- penguins |> 
  gg_point(
    x = flipper_length_mm, 
    y = body_mass_g, 
    y_include = 0, 
    x_breaks = scales::breaks_width(25))

p2 <- penguins |> 
  gg_point(
    x = flipper_length_mm, 
    y = body_mass_g, 
    y_limits = c(0, NA),
    x_breaks = scales::breaks_pretty(3))

p3 <- penguins |>
  gg_point(
    x = flipper_length_mm, 
    y = body_mass_g, 
    x_limits = c(190, 210),
    x_oob = scales::oob_keep,
    y_trans = "log10",
    y_limits = c(1000, NA),
    y_breaks = scales::breaks_width(1000),
    coord = coord_cartesian(clip = "on"))

p4 <- penguins |>
  gg_point(
    x = flipper_length_mm, 
    y = body_mass_g,
    x_trans = "reverse",
    x_limits = c(210, 190),
    x_breaks = scales::breaks_width(-10), 
    y_include = 0,
    y_trans = "sqrt")

(p1 + p2) / (p3 + p4)

## -----------------------------------------------------------------------------
penguins |>
  ggplot() +
  geom_blank(aes(x = flipper_length_mm, y = body_mass_g)) +
  labs(title = "Penguins body mass by flipper length",
       subtitle = " Palmer Archipelago, Antarctica",
       x = "Flipper length (mm)", 
       caption = "Source: Gorman, 2020")

## -----------------------------------------------------------------------------
penguins |>
  gg_blank(
    x = flipper_length_mm,
    y = body_mass_g, 
    title = "Penguins body mass by flipper length",
    subtitle = " Palmer Archipelago, Antarctica",
    x_title = "Flipper length (mm)",
    caption = "Source: Gorman, 2020")

## -----------------------------------------------------------------------------
penguins |>
  gg_point(x = flipper_length_mm,
           y = body_mass_g,
           col = sex,
           facet = species, 
           pal = c("#2596be", "#fc7c24"), 
           theme = theme_grey())

## -----------------------------------------------------------------------------
penguins |>
  gg_point(x = flipper_length_mm,
           y = body_mass_g,
           col = sex,
           facet = species, 
           pal = c("#2596be", "#fc7c24")) +
  theme_grey()

## -----------------------------------------------------------------------------
custom_theme <- gg_theme(
  text_size = 9,
  plot_background_pal = "#000000",
  panel_background_pal = "#232323",
  panel_grid_pal = "#000000",
  text_pal = "#d3d3d3"
)

penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g, 
    col = species, 
    theme = custom_theme) 

## -----------------------------------------------------------------------------
penguins |>
  group_by(species, sex) |> 
  summarise(across(body_mass_g, mean)) |> 
  ggplot() +
  geom_col(aes(x = species, y = body_mass_g, fill = sex),
    position = "dodge",
    width = 0.5
  )

## -----------------------------------------------------------------------------
penguins |>
  group_by(species, sex) |> 
  summarise(across(body_mass_g, mean)) |> 
  gg_col(
    x = species,
    y = body_mass_g,
    col = sex,
    position = "dodge",
    width = 0.5
  )

## -----------------------------------------------------------------------------
penguins |>
  group_by(species, sex) |> 
  summarise(across(body_mass_g, mean)) |> 
  ggplot() +
  geom_col(aes(x = body_mass_g, y = species, fill = sex),
           position = "dodge",
           width = 0.75
  )

## -----------------------------------------------------------------------------
penguins |>
  group_by(species, sex) |> 
  summarise(across(body_mass_g, mean)) |> 
  gg_col(
    x = body_mass_g,
    y = species,
    col = sex,
    position = "dodge",
    width = 0.75
  )

## -----------------------------------------------------------------------------
penguins |>
  gg_smooth(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    linewidth = 0.5, #accessed via geom_smooth
    level = 0.99, #accessed via geom_smooth
    colour = "white") #accessed via geom_smooth

## -----------------------------------------------------------------------------
penguins |>
  gg_smooth(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    linewidth = 0.5, #accessed via geom_smooth
    level = 0.99, #accessed via geom_smooth
    fill = "#a8a8a8") #accessed via geom_smooth

## -----------------------------------------------------------------------------
penguins |>
  gg_boxplot(x = species,
             y = body_mass_g,
             width = 0.5,
             outlier.colour = NA) +
  geom_jitter(colour = pal_blue)

## -----------------------------------------------------------------------------
penguins |>
  group_by(sex, species) |>
  summarise(across(body_mass_g, mean)) |>
  mutate(upper = body_mass_g * 1.05) |>
  mutate(lower = body_mass_g * 0.95) |>
  tidyr::drop_na(sex) %>% 
  gg_col(
    x = sex,
    y = body_mass_g,
    col = sex,
    facet = species,
    y_include = range(.$lower, .$upper),
    fill = "#d3d3d3",
    colour = "#d3d3d3", 
    width = 0.75) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1)

