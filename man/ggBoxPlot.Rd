% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggBoxPlot.R
\name{ggBoxPlot}
\alias{ggBoxPlot}
\title{Box plot.}
\usage{
ggBoxPlot(
  df,
  x,
  y,
  fill,
  facet = NULL,
  colors = NULL,
  ylab = "Y",
  yscale = "continuous",
  yscale.expand = F,
  facet_type = "wrap",
  facet_scales = "free_y",
  facet_space = "fixed",
  facet_ncol = NULL,
  facet_nrow = NULL
)
}
\arguments{
\item{df}{A data.frame of plot data.}

\item{x}{A string of the column name for x.}

\item{y}{A string of the column name for y.}

\item{fill}{A string of the column name for fill.}

\item{facet}{(Optional) A string or a pair of strings of the column name(s) for facet.}

\item{colors}{(Optional) A vector of colors.}

\item{ylab}{A string for y-axis label.}

\item{yscale}{The y-axis scale. Can be either "continuous" or "log10".}

\item{yscale.expand}{Logical. Whether the y-scale should be expanded down to zero.}

\item{facet_type}{The facetting type. Can be either "wrap" or "grid".}

\item{facet_scales}{The facetting scales.}

\item{facet_space}{The spacing scheme in the facetting grid.}

\item{facet_ncol}{Number of columns.}

\item{facet_nrow}{Number of rows.}
}
\description{
A box plot with outliers.
}
