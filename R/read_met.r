#' Read ARL packed meteorological data file layer
#' @author Ben Fasoli
#'
#' Extracts the given variable from file
#'
#' @param path to ARL packed file
#' @param var four digit character code of variable. Examples include SHGT PRSS
#'   MSLP TPP1 DIFR T02M DP2M PBLH DSWF ULWF U10M V10M VSBY CSNO CICE CFZR CRAI
#'   LIB4 CAPE CINH TCLD REFC CLDB CLDT
#' @param yy desired record year
#' @param mm desired record month of year
#' @param dd desired record day of month
#' @param hh desired record hour of day
#' @param lvl desired record vertical level (0:nz)
#' @param verbose print record data during fortran variable search
#' @param wnd_warning report grid orientation warning when querying wind data
#'
#' @return rasterLayer with associated projection metadata
#'
#' @useDynLib stiltread
#'
#' @export

read_met <- function(path, var, yy, mm, dd, hh, lvl, verbose = F,
                     wnd_warning = T) {

  if (!file.exists(path)) stop('File does not exist')
  var <- toupper(var)

  if (wnd_warning && var %in% c('UWND', 'VWND', 'U10M', 'V10M')) {
    warning(var, ' returned relative to native grid. See read_met_wind for ',
            'U and V wind relative to North and East respectively or set ',
            'wnd_warning = F to suppress this message.')
  }

  meta <- stiltread::read_met_header(path)

  # Extract desired variable grid
  out <- .Fortran('fetch_grid', PACKAGE = 'stiltread',
                  file = path,
                  var = var,
                  nx = meta$nx,
                  ny = meta$ny,
                  yy = as.integer(yy),
                  mm = as.integer(mm),
                  dd = as.integer(dd),
                  hh = as.integer(hh),
                  lvl = as.integer(lvl),
                  verbose = as.integer(verbose),
                  rdata = array(0, dim = c(meta$nx, meta$ny)))

  raster::raster(apply(t(out$rdata), 2, rev),
                 crs = as.character(meta$crs),
                 xmn = meta$crs_domain[1],
                 xmx = meta$crs_domain[2],
                 ymn = meta$crs_domain[3],
                 ymx = meta$crs_domain[4]
  )
}
