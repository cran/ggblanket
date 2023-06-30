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
library(ggblanket)
library(patchwork)

penguins <- palmerpenguins::penguins |>
  mutate(sex = stringr::str_to_sentence(sex)) |>
  tidyr::drop_na(sex)

## -----------------------------------------------------------------------------
# ggplot2
penguins |> 
  ggplot() + 
  geom_point(aes(x = flipper_length_mm, 
                 y = body_mass_g))

## -----------------------------------------------------------------------------
# ggblanket
penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g)

## -----------------------------------------------------------------------------
# ggplot2
p1 <- penguins |> 
  ggplot() + 
  geom_point(aes(x = flipper_length_mm, 
                 y = body_mass_g,
                 col = species)) + 
  theme(legend.position = "bottom") +
  guides(col = guide_legend(title.position = "top")) +
  labs(col = "Species") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3))

p2 <- penguins |>
  ggplot() +
  geom_density(aes(x = body_mass_g, 
                   fill = species)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top")) +
  labs(fill = "Species")

p1 + p2

## -----------------------------------------------------------------------------
# ggblanket
p1 <- penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g, 
    col = species, 
    x_breaks = scales::breaks_pretty(n = 3), 
    col_legend_ncol = 2)

p2 <- penguins |>
  gg_density(
    x = body_mass_g, 
    col = species,
    x_breaks = scales::breaks_pretty(n = 3), 
    col_legend_ncol = 2)

p1 + p2

## -----------------------------------------------------------------------------
# ggplot2
p1 <- penguins |>
  ggplot() +
  geom_histogram(aes(x = body_mass_g),
                 fill = "#1B9E77") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) 

p2 <- penguins |>
  ggplot() +
  geom_jitter(aes(x = species, 
                  y = body_mass_g, 
                  col = sex)) +
  scale_color_manual(values = c("#2596be", "#fc7c24"))

p1 + p2

## -----------------------------------------------------------------------------
# ggblanket
p1 <- penguins |>
  gg_histogram(
    x = body_mass_g, 
    pal = "#1B9E77", 
    x_breaks = scales::breaks_pretty(n = 3))

p2 <- penguins |>
  gg_jitter(
    x = species, 
    y = body_mass_g, 
    col = sex, 
    pal = c("#2596be", "#fc7c24"))

p1 + p2

## -----------------------------------------------------------------------------
# ggplot2
penguins |>
  ggplot() +
  geom_violin(aes(x = sex, 
                  y = body_mass_g)) +
  facet_wrap(~species) 

## -----------------------------------------------------------------------------
# ggblanket
penguins |>
  gg_violin(
    x = sex,
    y = body_mass_g,
    facet = species)

## ---- fig.asp=0.75------------------------------------------------------------
# ggplot2
penguins |>
  ggplot() +
  geom_histogram(aes(x = flipper_length_mm)) +
  facet_grid(species ~ sex)

## -----------------------------------------------------------------------------
# ggblanket
penguins |>
  gg_histogram(
    x = flipper_length_mm,
    facet = sex,
    facet2 = species)

## ---- echo = FALSE,   fig.width = 3, fig.asp = 2------------------------------
knitr::include_graphics("screenshot_autotab_y.png", dpi = 300)

## -----------------------------------------------------------------------------
# ggplot2
penguins |>
  ggplot() +
  geom_jitter(aes(x = species, 
                  y = body_mass_g, 
                  col = sex)) +
  expand_limits(y = 0) +
  scale_x_discrete(labels = \(x) stringr::str_sub(x, 1, 1)) +
  scale_y_continuous(breaks = scales::breaks_width(1500),
                     labels = scales::label_number(big.mark = " "),
                     trans = "sqrt") +
  labs(x = "Species", y = "Body mass (g)", col = "Sex") +
  theme(legend.position = "top") +
  theme(legend.justification = "left") +
  scale_colour_manual(values = scales::hue_pal()(2), 
                      guide = ggplot2::guide_legend(title.position = "top"))

## -----------------------------------------------------------------------------
# ggblanket
penguins |>
  gg_jitter(
    x = species,
    y = body_mass_g,
    col = sex,
    x_labels = \(x) stringr::str_sub(x, 1, 1),
    y_include = 0,
    y_breaks = scales::breaks_width(1500), 
    y_labels = scales::label_number(big.mark = " "), 
    y_trans = "sqrt",
    y_title = "Body mass (g)", 
    col_legend_place = "t")

## -----------------------------------------------------------------------------
# ggplot2
penguins |>
  ggplot() +
  geom_point(aes(x = flipper_length_mm, 
                 y = body_mass_g, 
                 col = sex)) +
  facet_wrap(~species) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) 

## -----------------------------------------------------------------------------
# ggblanket
penguins |>
  gg_point(
      x = flipper_length_mm,
      y = body_mass_g, 
      col = sex,
      facet = species)

## ---- fig.asp=0.7-------------------------------------------------------------
# ggblanket
theme_set(dark_mode())

penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    title = "Penguins body mass by flipper length",
    subtitle = "Palmer Archipelago, Antarctica",
    caption = "Source: Gorman, 2020",
    pal = c("#2596be", "#fc7c24"))

## ---- fig.asp=0.75------------------------------------------------------------
# ggblanket
theme_set(light_mode(base_size = 12))

penguins |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    title = "Penguins body mass by flipper length",
    subtitle = "Palmer Archipelago, Antarctica",
    caption = "Source: Gorman, 2020",
    pal = c("#2596be", "#fc7c24"))
theme_set(theme_grey()) #unset the theme

## -----------------------------------------------------------------------------
# ggplot2
penguins |>
  group_by(species, sex) |> 
  summarise(body_mass_g = mean(body_mass_g)) |> 
  ggplot() +
  geom_col(aes(x = body_mass_g, 
               y = species, 
               fill = sex),
           position = "dodge",
           width = 0.75)

## -----------------------------------------------------------------------------
# ggblanket
penguins |>
  group_by(species, sex) |> 
  summarise(body_mass_g = mean(body_mass_g)) |> 
  gg_col(
    x = body_mass_g,
    y = species,
    col = sex,
    position = "dodge",
    width = 0.75)

## -----------------------------------------------------------------------------
# ggblanket
penguins |>
  gg_smooth(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    linewidth = 0.5, #accessed via geom_smooth
    level = 0.99) #accessed via geom_smooth

## -----------------------------------------------------------------------------
# ggblanket + ggplot2
penguins |>
  gg_boxplot(x = species,
             y = body_mass_g,
             width = 0.5,
             outlier.colour = NA) +
  geom_jitter(col = pal_blue)

## ---- fig.asp=0.75------------------------------------------------------------
# ggblanket + ggplot2
d <- penguins |>
  group_by(species) |>
  summarise(body_mass_g = mean(body_mass_g)) |>
  mutate(lower = body_mass_g * 0.95) |> 
  mutate(upper = body_mass_g * 1.2)

p1 <- d |>
  gg_blank(    
    y = species,
    x = body_mass_g,
    col = species,
    xmin = lower,
    xmax = upper,
    x_include = 0, 
    x_labels = \(x) x / 1000, 
    x_title = "Body mass kg") +
  geom_col(alpha = 0.9, width = 0.75) +
  geom_errorbar(col = "black", width = 0.1)

p2 <- d |>
  gg_blank(
    y = species,
    x = body_mass_g,
    xmin = lower, 
    xmax = upper, 
    col = species,
    x_include = 0, 
    x_labels = \(x) x / 1000, 
    x_title = "Body mass kg") +
  geom_col(col = NA, fill = "#d3d3d3", width = 0.75) +
  geom_errorbar(width = 0.1)

p1 / p2

## -----------------------------------------------------------------------------
# ggblanket + ggplot2
d_wide <- gapminder::gapminder |>
  filter(year %in% c(1967, 2007)) |>
  select(country, year, lifeExp) |>
  tidyr::pivot_wider(names_from = year, values_from = lifeExp) |>
  mutate(gap = `2007` - `1967`) |>
  slice_max(gap, n = 10) |>
  mutate(country = forcats::fct_inorder(forcats::fct_drop(country))) 

d_long <- d_wide |>
  select(-gap) |> 
  tidyr::pivot_longer(-country, 
                      names_to = "year", 
                      values_to = "life_expectancy")

d_long |> 
  gg_blank(x = life_expectancy,
           y = country,
           col = year,
           pal = pal_hue[c(2, 1)],
           x_include = 0,
           col_legend_place = "r",
           title = "We're living longer",
           subtitle = "Biggest life expectancy rise, 1967\u20132007",
           x_title = "Life expectancy", 
           y_title = "") +
  geom_segment(aes(x = `1967`, xend = `2007`, 
                   y = country, yend = country), 
               data = d_wide, inherit.aes = FALSE, 
               col = "#dddddd", linewidth = 2) +
  geom_point(size = 2) 

