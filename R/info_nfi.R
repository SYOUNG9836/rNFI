#' @import dplyr
#' @import BiodiversityR
#' @import sf
#' @import plotrix
#' @import rlang
#' @importFrom magrittr %>%
#' @importFrom stats var
#' @importFrom utils install.packages
NULL


.onLoad <- function(libname, pkgname) {
  repos = getOption("repos")
  repos["kadmin"] = "https://SYOUNG9836.github.io/drat/"
  options(repos = repos)
  invisible(repos)
}
