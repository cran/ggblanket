% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mode_defaults.R
\docType{data}
\name{lightness}
\alias{lightness}
\alias{darkness}
\title{Mode colour and linewidth defaults}
\usage{
lightness

darkness
}
\value{
A character vector.
}
\description{
\code{lightness} and \code{darkness} are vectors of 3 colours used in the \verb{*_theme_*} themes for the for the text, axis.line (and axis.ticks), panel.grid, panel.background and plot.background etc.

\code{linewidthness} is a vector of 2 integers used in the \verb{*_theme_*} themes for the linewidth of the axis.line (axis.ticks and legend.ticks) and panel.grid theme elements.
}
\examples{
scales::show_col(c(lightness, darkness), ncol = 3)
}
\keyword{datasets}
