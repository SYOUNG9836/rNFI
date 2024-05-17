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


# .onLoad <- function(libname, pkgname) {
#   repos = getOption("repos")
#   repos["kadmin"] = "https://SYOUNG9836.github.io/drat/"
#   options(repos = repos)
#   invisible(repos)
# }


.onAttach <- function(lib, pkg) {
  if(interactive() || getOption("verbose"))
    packageStartupMessage(sprintf("Package %s (%s) loaded. Check out our website at https://github.com/SYOUNG9836/rNFI.\nType citation(\"%s\") for examples of how to cite rFIA.\n", pkg,
                                  packageDescription(pkg)$Version, pkg))
}
