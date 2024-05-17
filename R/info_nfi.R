#' @import dplyr
#' @import BiodiversityR
#' @import sf
#' @import plotrix
#' @import readxl
#' @import stringr
#' @import vegan
#' @import cellranger
#' @import broom
#' @import ggplot2
#' @import sp
#' @import cowplot
#' @import drat
#' @importFrom magrittr %>%
#' @importFrom stats var
#' @importFrom utils install.packages
#' @importFrom utils packageDescription
#' @importFrom utils menu
#' @importFrom data.table rbindlist
#' @importFrom rlang sym
#' @importFrom rlang syms
#' @importFrom rlang parse_exprs
#' @importFrom tidyr spread
#' 
NULL

.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  has_data <- requireNamespace("kadmin", quietly = TRUE)
  .pkgenv[["kadmin"]] <- has_data
  # repos = getOption("repos")
  # repos["kadmin"] = "https://SYOUNG9836.github.io/drat/"
  # options(repos = repos)
  # invisible(repos)
}


.onAttach <- function(libname, pkgname) {
  if (!.pkgenv$has_data) {
    msg <- paste("To access larger datasets in this package, install the",
                 "kadmin package with:\n",
                 "`install.packages('kadmin',", 
                 "repos='https://SYOUNG9836.github.io/drat/', type='source')`")
    msg <- paste(strwrap(msg), collapse="\n")
    packageStartupMessage(msg)
  }
  
  if(interactive() || getOption("verbose"))
    packageStartupMessage(sprintf("Package %s (%s) loaded. Check out our website at https://github.com/SYOUNG9836/rNFI.\nType citation(\"%s\") for examples of how to cite rFIA.\n", pkgname,
                                  packageDescription(pkgname)$Version, pkgname))
}

hasData <- function(has_data = .pkgenv$has_data) {
  if (!has_data) {
    msg <- paste("To use this function, you must have the",
                 "`kadmin` package installed.")
    msg <- paste(strwrap(msg), collapse="\n")
    stop(msg)
  }
}


