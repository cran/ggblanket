## ----include = FALSE----------------------------------------------------------
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

penguins2 <- palmerpenguins::penguins |>
  mutate(sex = stringr::str_to_sentence(sex)) |>
  tidyr::drop_na(sex)

## ----fig.asp=0.55-------------------------------------------------------------
# ggplot2
penguins2 |> 
  ggplot() + 
  geom_point(aes(x = flipper_length_mm, 
                 y = body_mass_g))

## ----fig.asp=0.55-------------------------------------------------------------
# ggblanket
penguins2 |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g)

## ----fig.asp=0.85-------------------------------------------------------------
# ggplot2
p1 <- penguins2 |> 
  ggplot() + 
  geom_point(aes(x = flipper_length_mm, 
                 y = body_mass_g,
                 colour = species)) 

p2 <- penguins2 |>
  ggplot() +
  geom_density(aes(x = flipper_length_mm, 
                   fill = species)) +
  labs(fill = "Species")

p1 / p2

## ----fig.asp=1.05-------------------------------------------------------------
# ggblanket
p1 <- penguins2 |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g, 
    col = species)

p2 <- penguins2 |>
  gg_density(
    x = flipper_length_mm, 
    col = species)

p1 / p2

## ----fig.asp=0.85-------------------------------------------------------------
# ggplot2
p1 <- penguins2 |>
  ggplot() +
  geom_histogram(aes(x = body_mass_g),
                 fill = "#414D6B")

p2 <- penguins2 |>
  ggplot() +
  geom_jitter(aes(x = species, 
                  y = body_mass_g, 
                  colour = sex)) +
  scale_colour_manual(values = c("#1B9E77", "#9E361B"))

p1 / p2

## ----fig.asp=0.95-------------------------------------------------------------
# ggblanket
p1 <- penguins2 |>
  gg_histogram(
    x = body_mass_g, 
    pal = "#414D6B")

p2 <- penguins2 |>
  gg_jitter(
    x = species, 
    y = body_mass_g, 
    col = sex, 
    pal = c("#1B9E77", "#9E361B"))

p1 / p2

## ----fig.asp=0.55-------------------------------------------------------------
# ggplot2
penguins2 |>
  ggplot() +
  geom_violin(aes(x = sex, 
                  y = body_mass_g)) +
  facet_wrap(vars(species)) 

## ----fig.asp=0.55-------------------------------------------------------------
# ggblanket
penguins2 |>
  gg_violin(
    x = sex,
    y = body_mass_g,
    facet = species)

## ----fig.asp=0.75-------------------------------------------------------------
# ggplot2
penguins2 |>
  ggplot() +
  geom_histogram(aes(x = flipper_length_mm)) +
  facet_grid(rows = vars(sex), cols = vars(species))

## ----fig.asp=0.75-------------------------------------------------------------
# ggblanket
penguins2 |>
  gg_histogram(
    x = flipper_length_mm,
    facet = species,
    facet2 = sex)

## ----fig.asp=0.525------------------------------------------------------------
# ggplot2
penguins2 |>
  ggplot() +
  geom_point(aes(x = flipper_length_mm, 
                 y = body_mass_g, 
                 colour = sex)) +
  facet_wrap(vars(species)) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) 

## -----------------------------------------------------------------------------
# ggblanket
penguins2 |>
  gg_point(
      x = flipper_length_mm,
      y = body_mass_g, 
      col = sex,
      facet = species)

## ----fig.asp=0.6--------------------------------------------------------------
# ggplot2
penguins2 |>
  ggplot() +
  geom_jitter(aes(x = species, 
                  y = body_mass_g, 
                  colour = sex)) +
  expand_limits(y = 0) +
  scale_x_discrete(labels = \(x) stringr::str_sub(x, 1, 1)) +
  scale_y_continuous(breaks = scales::breaks_width(1500),
                     labels = scales::label_number(big.mark = " "),
                     expand = expansion(mult = c(0, 0.05)),
                     trans = "sqrt") +
  labs(x = "Species", y = "Body mass (g)", col = NULL) +
  theme(legend.position = "top") +
  theme(legend.justification = "left") +
  scale_colour_manual(values = scales::hue_pal()(2), 
                      guide = ggplot2::guide_legend(title.position = "top"))

## ----fig.asp=0.55-------------------------------------------------------------
# ggblanket
penguins2 |>
  gg_jitter(
    x = species,
    y = body_mass_g,
    col = sex,
    x_labels = \(x) stringr::str_sub(x, 1, 1),
    y_include = 0,
    y_breaks = scales::breaks_width(1500), 
    y_labels = scales::label_number(big.mark = " "), 
    y_expand = expansion(mult = c(0, 0.05)),
    y_trans = "sqrt",
    y_title = "Body mass (g)", 
    col_legend_place = "t", 
    col_title = "")

## ----echo = FALSE,   fig.width = 3, fig.asp = 2-------------------------------
knitr::include_graphics("autocomplete_y.png", dpi = 300)

## ----fig.asp=0.75-------------------------------------------------------------
# ggblanket

# theme_set(dark_mode(10))

penguins2 |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    title = "Penguins body mass by flipper length",
    subtitle = "Palmer Archipelago, Antarctica",
    caption = "Source: Gorman, 2020", 
    theme = dark_mode(10))

## -----------------------------------------------------------------------------
# ggblanket
p1 <- penguins2 |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    x_breaks = scales::breaks_pretty(n = 3),
    theme = theme_grey(),
    title = "theme= theme_grey()")

p2 <- penguins2 |>
  gg_point(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex,
    x_breaks = scales::breaks_pretty(n = 3),
    title = "+ theme_grey()") +
  theme_grey()

p1 + p2

## -----------------------------------------------------------------------------
# ggblanket
penguins2 |>
  gg_smooth(
    x = flipper_length_mm,
    y = body_mass_g,
    col = sex, 
    linewidth = 0.5, #accessed via geom_smooth
    level = 0.99) #accessed via geom_smooth

## ----fig.asp=0.55-------------------------------------------------------------
# ggplot2
penguins2 |>
  group_by(species, sex) |> 
  summarise(body_mass_g = mean(body_mass_g)) |> 
  ggplot() +
  geom_col(aes(x = body_mass_g, 
               y = species, 
               fill = sex),
           position = "dodge",
           width = 0.66)

## -----------------------------------------------------------------------------
# ggblanket
penguins2 |>
  group_by(species, sex) |> 
  summarise(body_mass_g = mean(body_mass_g)) |> 
  gg_col(
    x = body_mass_g,
    y = species,
    col = sex,
    position = "dodge",
    width = 0.66)

## -----------------------------------------------------------------------------
# ggblanket
penguins2 |>
  group_by(species, sex) |> 
  summarise(body_mass_g = mean(body_mass_g)) |> 
  ungroup() |> 
  gg_col(
    y = body_mass_g,
    x = species,
    col = sex,
    position = "dodge",
    width = 0.5,
    x_labels = \(x) stringr::str_sub(x, 1, 1),
    y_labels = \(x) str_keep_seq(x),
    title = "Keep every 2nd label", 
    theme = light_mode(title_face = "plain"))

## -----------------------------------------------------------------------------
# ggblanket + ggplot2
p1 <- ggplot2::economics |>
  slice_min(order_by = date, n = 10) |> 
  gg_line(
    x = date,
    y = unemploy,
    pal = guardian()[1],
    x_title = "",
    y_title = "Unemployment",
    y_include = 0,
    linewidth = 1, 
    x_breaks = scales::breaks_width("3 months"),
    title = "gg_line + geom_point", 
    theme = light_mode(title_face = "plain")) +
  geom_point(colour = guardian()[2])

p2 <- ggplot2::economics |>
  slice_min(order_by = date, n = 10) |> 
  gg_point(
    x = date,
    y = unemploy,
    pal = guardian()[2],
    x_title = "",
    y_title = "Unemployment",
    y_include = 0,
    x_breaks = scales::breaks_width("3 months"),
    title = "gg_point + geom_line", 
    theme = light_mode(title_face = "plain")) +
  geom_line(colour = guardian()[1], linewidth = 1)

p1 + p2

## ----fig.asp=0.75-------------------------------------------------------------
# ggblanket + ggplot2
d <- penguins2 |>
  group_by(species) |>
  summarise(body_mass_g = mean(body_mass_g)) |>
  mutate(lower = body_mass_g * 0.95) |> 
  mutate(upper = body_mass_g * 1.2)

p1 <- d |>
  gg_col(
    x = species,
    y = body_mass_g,
    col = species,
    width = 0.75,
    y_include = c(0, max(d$upper)), 
    y_labels = \(x) x / 1000, 
    y_title = "Body mass kg", 
    col_legend_place = "n") +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                colour = "black",
                width = 0.1) +
  coord_flip()

p2 <- d |>
  gg_col(
    x = species,
    y = body_mass_g,
    col = species,
    colour = "#d3d3d3",
    fill = "#d3d3d3",
    width = 0.75,
    y_include = c(0, max(d$upper)), 
    y_labels = \(x) x / 1000, 
    y_title = "Body mass kg", 
    col_legend_place = "n") +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.1) +
  coord_flip()

p1 / p2

## ----fig.asp=0.75-------------------------------------------------------------
p1 <- diamonds |>
  count(color) |>
  gg_col(
    x = n,
    y = color,
    width = 0.75,
    x_labels = \(x) x / 1000,
    x_title = "Count (thousands)",
    title = "Default y",
    theme = light_mode(title_face = "plain")
  )


p2 <- diamonds |>
  count(color) |>
  mutate(color = forcats::fct_rev(color)) |>
  gg_col(
    x = n,
    y = color,
    width = 0.75,
    x_labels = \(x) x / 1000,
    x_title = "Count (thousands)",
    title = "Reverse y",
    theme = light_mode(title_face = "plain")
  )

p3 <- diamonds |>
  count(color) |>
  mutate(color = forcats::fct_reorder(color, n)) |>
  gg_col(
    x = n,
    y = color,
    width = 0.75,
    x_labels = \(x) x / 1000,
    x_title = "Count (thousands)",
    title = "Reordered y ascending by x",
    theme = light_mode(title_face = "plain")
  )

p4 <- diamonds |>
  count(color) |>
  mutate(color = color |>
           forcats::fct_reorder(n) |>
           forcats::fct_rev()) |>
  gg_col(
    x = n,
    y = color,
    width = 0.75,
    x_labels = \(x) x / 1000,
    x_title = "Count (thousands)",
    title = "Reordered y decending by x",
    theme = light_mode(title_face = "plain")
  )

(p1 + p2) / (p3 + p4)

## ----fig.asp=0.4--------------------------------------------------------------
p1 <- diamonds |> 
  count(color) |> 
  filter(color %in% c("E", "G", "I")) |>
  gg_point(
    x = n,
    y = color,
    x_labels = \(x) x / 1000,
    x_title = "Count (thousands)",
    title = "A factor filtered",
    theme = light_mode(title_face = "plain"))

p2 <- diamonds |> 
  count(color) |>
  filter(color %in% c("E", "G", "I")) |>
  mutate(color = forcats::fct_drop(color)) |> 
  gg_point(
    x = n,
    y = color,
    x_labels = \(x) x / 1000,
    x_title = "Count (thousands)",
    title = "A factor filtered & unused levels dropped",
    theme = light_mode(title_face = "plain"))

p1 + p2

## ----fig.asp=1----------------------------------------------------------------
p1 <- economics |> 
  gg_smooth(
    x = date, 
    y = unemploy, 
    y_labels = \(x) str_keep_seq(x),
    title = "No x_limits set", 
    theme = light_mode(title_face = "plain")) +
  geom_vline(xintercept = c(lubridate::ymd("1985-01-01", "1995-01-01")),
             col = guardian(n = 1), 
             linetype = 3) +
  geom_point(col = guardian(n = 1), alpha = 0.05)

p2 <- economics |> 
  gg_smooth(
    x = date, 
    y = unemploy, 
    x_limits = c(lubridate::ymd("1985-01-01", "1995-01-01")),
    x_labels = \(x) stringr::str_sub(x, 3, 4),
    y_labels = \(x) str_keep_seq(x),
    title = "x_limits set", 
    theme = light_mode(title_face = "plain")) +
  geom_point(col = guardian(n = 1), alpha = 0.1)

p3 <- economics |> 
  gg_smooth(
    x = date, 
    y = unemploy, 
    x_limits = c(lubridate::ymd("1985-01-01", "1995-01-01")),
    x_labels = \(x) stringr::str_sub(x, 3, 4),
    y_labels = \(x) str_keep_seq(x),
    coord = coord_cartesian(clip = "on"), 
    title = "x_limits set & cartesian space clipped", 
    theme = light_mode(title_face = "plain")) +
  geom_point(col = guardian(n = 1), alpha = 0.1)

p4 <- economics |> 
  gg_smooth(
    x = date, 
    y = unemploy, 
    x_limits = c(lubridate::ymd("1985-01-01", "1995-01-01")),
    x_labels = \(x) stringr::str_sub(x, 3, 4),
    x_oob = scales::oob_censor,
    y_labels = \(x) str_keep_seq(x),
    title = "x_limits set & x_oob censored", 
    theme = light_mode(title_face = "plain")) +
  geom_point(col = guardian(n = 1), alpha = 0.1)

p5 <- economics |> 
  filter(between(date, lubridate::ymd("1985-01-01"), lubridate::ymd("1995-01-01"))) |> 
  gg_smooth(
    x = date, 
    y = unemploy,
    x_labels = \(x) stringr::str_sub(x, 3, 4),
    y_labels = \(x) str_keep_seq(x),
    title = "x data filtered", 
    theme = light_mode(title_face = "plain")) +
  geom_point(col = guardian(n = 1), alpha = 0.1)

p1 / (p2 + p3) / (p4 + p5) 

## -----------------------------------------------------------------------------
bench::mark({
  penguins2 |> 
    gg_point(x = flipper_length_mm, 
             y = body_mass_g, 
             col = species)
}, iterations = 10)

## -----------------------------------------------------------------------------
bench::mark({
  penguins2 |>
    ggplot() +
    geom_point(aes(x = flipper_length_mm, y = body_mass_g, colour = species)) 
}, iterations = 10)

