% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_NFI.R
\name{summary_NFI}
\alias{summary_NFI}
\title{Calculates descriptive statistics}
\usage{
summary_NFI(
  data,
  grpby = NULL,
  byplot = FALSE,
  clusterplot = FALSE,
  largetreearea = TRUE,
  Stockedland = TRUE,
  talltree = TRUE
)
}
\arguments{
\item{data}{: A \code{list} generated by \code{\link{read_NFI}} that contains 'plot' and 'tree' data frames.}

\item{grpby}{: A character vector indicating variables from 'plot' tables for grouping. Use \code{c()} to combine multiple variables.}

\item{byplot}{: A logical flag indicating whether to calculate for each plot separately or for the entire dataset.}

\item{clusterplot}{: A logical flag indicating whether to calculate for cluster plot collectively or calculate for each subplot separately.}

\item{largetreearea}{: A logical flag indicating whether to include a large tree plot as well, or only a tree plot.}

\item{Stockedland}{: A logical flag indicating whether to include only stocked land or also include other types of land.}

\item{talltree}{: A logical flag indicating whether to include only tall trees or also shrubs.}
}
\value{
A \code{data.frame} that includes summary statistics for plot or study areas
}
\description{
summary_NFI() is a function that calculates descriptive statistics for each plot, the entire study area, or groups within the study area.
You can specify whether to include large tree survey plots, to focus only on tall trees and Stocked land, and to treat cluster plots as single plots.
}
\details{
num_tree, num_largetree, num_dominanttree, num_species,
num_clusterplot, num_subplot, num_largetree_subplot, num_dbh30_subplot,
mean_DBH, se_DBH, mean_H, se_H, mean_dominant_H, se_dominant_H
mean_tree_n_ha, se_tree_n_ha, mean_basal_m2_ha, se_basal_m2_ha, mean_volume_m3_ha, se_volume_m3_ha
}
\examples{
\dontrun{
summary_NFI(NFI5)
}

}
