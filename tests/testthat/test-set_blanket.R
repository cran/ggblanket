testthat::skip_if(getRversion() <= package_version("4.1.0"))
testthat::skip_on_os(c("mac", "linux"))

library(palmerpenguins)
library(ggplot2)
library(dplyr)

###
test_name <- "1"

test_that(test_name, {

  set_blanket(
    mode = NULL,
  )
  weave_theme(theme = theme_grey())

  p <- penguins |>
    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    geom_vline(xintercept = 200)

  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "2"

test_that(test_name, {

  set_blanket(
    mode = light_mode_r(),
    colour = red,
    text_colour = teal,
    reference_colour = teal,
  )

  p <- penguins |>
    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = sex,
    ) +
    geom_vline(xintercept = 200)

  vdiffr::expect_doppelganger(test_name, p)
})

###

set_blanket(
  mode = dark_mode_r(base_size = 15),
  colour = red,
  text_colour = "red",
  text_size = 15 / 2.83505,
  reference_colour = "red",
  reference_linewidth = 5,
  col_palette_d = c(navy, red, "green"),
  col_palette_c = c(navy, purple, red, orange)
)

###
test_name <- "3"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      x_breaks = scales::breaks_pretty(3),
    ) +
    geom_vline(xintercept = 200) +
    annotate("text", x = I(0.25), y = I(0.75), label = "Here")

  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "4"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      x_breaks = scales::breaks_pretty(3),
    ) +
    geom_vline(xintercept = 200) +
    annotate("text",
             x = I(0.25),
             y = I(0.75),
             label = "Here")


  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "5"

test_that(test_name, {

  p <- penguins |>
    gg_point(
      x = flipper_length_mm,
      y = body_mass_g,
      col = bill_depth_mm,
      x_breaks = scales::breaks_pretty(3),
    ) +
    geom_vline(xintercept = 200) +
    annotate("text", x = I(0.25), y = I(0.75), label = "Here")

  vdiffr::expect_doppelganger(test_name, p)
})

###
test_name <- "6"

test_that(test_name, {

  p <- penguins |>
    mutate(across(sex, \(x) stringr::str_to_sentence(x))) |>
    gg_smooth(
      x = flipper_length_mm,
      y = body_mass_g,
      col = species,
      se = TRUE,
    ) +
    geom_vline(xintercept = 200) +
    annotate("text", x = I(0.25), y = I(0.75), label = "Here")

  vdiffr::expect_doppelganger(test_name, p)
})

set_blanket()
