
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggblanket <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggblanket)](https://CRAN.R-project.org/package=ggblanket)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/ggblanket?color=lightgrey)](https://r-pkg.org/pkg/ggblanket)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-month/ggblanket?color=lightgrey)](https://r-pkg.org/pkg/ggblanket)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-week/ggblanket?color=lightgrey)](https://r-pkg.org/pkg/ggblanket)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/last-day/ggblanket?color=lightgrey)](https://r-pkg.org/pkg/ggblanket)
<!-- badges: end -->

## Overview

ggblanket is a package of ggplot2 wrapper functions.

The primary objective is to **simplify ggplot2 visualisation**.

Secondary objectives relate to:

- Design: produce well-designed visualisation by default
- Scope: cover the most useful 80% of what ggplot2 does
- Alignment: use conventions generally aligned with ggplot2.

## Installation

``` r
install.packages("ggblanket")
```

## Example

``` r
library(dplyr)
library(ggblanket)
library(palmerpenguins)

penguins |>
  tidyr::drop_na(sex) |> 
  mutate(sex = stringr::str_to_sentence(sex)) |>
  gg_histogram(
    x = flipper_length_mm,
    col = sex,
    facet = species)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="75%" />

## Get started

Click
[here](https://davidhodge931.github.io/ggblanket/articles/ggblanket.html)
to start learning how ggblanket works.

## Thanks!

Thank you to all authors of ggplot2, tidyverse, and the wider R
ecosystem.

If you like ggblanket, please give the repository a star.
