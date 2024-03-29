% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_e61.R
\name{theme_e61_alt}
\alias{theme_e61_alt}
\title{e61 themed graph options in an alternative style}
\usage{
theme_e61_alt(base_family = "pt-sans", base_size = 12)
}
\arguments{
\item{base_family}{Character. Chart font family. Default is Arial.}

\item{base_size}{Numeric. Chart font size. Default is 12.}
}
\value{
ggplot2 object
}
\description{
e61 themed graph options in an alternative style
}
\examples{
ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  theme_e61_alt()
}
