% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tsvis_NFI.R
\name{tsvis_NFI}
\alias{tsvis_NFI}
\title{Visualizes time series data}
\usage{
tsvis_NFI(
  data,
  admin = NULL,
  y = "biomass",
  bm_type = NULL,
  output = "line",
  strat = "FORTYP_SUB",
  clusterplot = FALSE,
  largetreearea = TRUE,
  Stockedland = TRUE,
  talltree = TRUE,
  sp = "SP",
  frequency = TRUE
)
}
\arguments{
\item{data}{: A \code{list} generated by \code{\link{read_NFI}} that contains 'plot' and 'tree' data frames.}

\item{admin}{: A character vector; the administrative unit for visualizing 'biomass' or 'cwd' as a map. Must be one of 'sido', 'sgg', 'emg'.}

\item{y}{: A character vector; the variable to visualize. Must be one of 'biomass', 'cwd', 'iv'.}

\item{bm_type}{: A character vector; the specific variable to visualize for 'biomass' or 'cwd'. Must be one of 'volume', 'biomass', 'AG_biomass', 'carbon', 'co2'.}

\item{output}{: A character vector; the desired type of visualization. Must be one of 'table', 'line', 'bar', 'map'.}

\item{strat}{: A character vector; the variable used for post-stratification. In the National Forest Inventory of Korea, it is typically used by forest type.}

\item{clusterplot}{: A logical flag; whether to calculate for cluster plot collectively or calculate for each subplot separately.}

\item{largetreearea}{: A logical flag; whether to include a large tree plot as well, or only a tree plot.}

\item{Stockedland}{: A logical flag; whether to include only stocked land or also include other types of land.}

\item{talltree}{: A logical flag; whether to include only tall trees or also shrubs.}

\item{sp}{: A character vector; the column name of tree species.}

\item{frequency}{: A logical flag; whether to use frequency in importance calculations.}
}
\value{
plot data
}
\description{
tsvis_NFI() is a function that analyzes and visualizes data in a time series format. It can visualize 'biomass', 'cwd', and 'iv' data as 'table', 'line', 'bar', or 'map'.
you need to select the specific biomass variable, such as volume or carbon, to visualize biomass.
If you want to view biomass on a map, you must choose the administrative unit level for the map visualization.
}
\note{
To visualize data as a map, you need to agree to install the \code{kadmin} package during the function execution or install it in advance.
The \code{kadmin} package loads shapefiles for Korea's Si, Do or Si, Gun, Gu or Eup, Myeon, Dong.
\verb{drat::addRepo("SYOUNG9836") install.packages("kadmin")} or \code{remotes::install_github("SYOUNG9836/kadmin")}
}
\examples{
\dontrun{
tsvis_NFI(NFI5, y="iv", output="bar")
tsvis_NFI(NFI5, admin="sido", y="biomass", bm_type="carbon", output="map")
}

}
\seealso{
\code{\link[rNFI]{iv_NFI}} for calculating the importance values.
\code{\link[rNFI]{biomass_NFI}} for calculating the biomass.
\code{\link[rNFI]{cwd_biomass_NFI}} for calculating the biomass of Coarse Woody Debris.
}
