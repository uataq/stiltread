#' Install stiltread dependencies
#' @author Ben Fasoli
#'
#' Run after installing stiltread to set up dependencies on system
#'
#' @import reticulate
#' @export

install_dependencies <- function(...) {
  py_install('wrf-python')
}
