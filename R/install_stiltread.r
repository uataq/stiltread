#' Install stiltread dependencies
#' @author Ben Fasoli
#'
#' Run after installing stiltread to set up dependencies on system
#'
#' @export

install_stiltread <- function() {
  reticulate::py_install('wrf-python')
}
