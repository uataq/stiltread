#' Read ARL packed meteorological data file winds
#' @author Ben Fasoli
#'
#' Extracts the winds at given level from file. For the surface level (0), 10m
#' u and v winds are reported.
#'
#' @param path to ARL packed file
#' @param yy desired record year
#' @param mm desired record month of year
#' @param dd desired record day of month
#' @param hh desired record hour of day
#' @param lvl desired record vertical level (0:nz)
#'
#' @return rasterStack with u and v rasterLayers projected into '+proj=longlat'
#'
#' @useDynLib stiltread
#'
#' @export

read_met_wind <- function(path, yy, mm, dd, hh, lvl, wnd_warning = T) {

  # Read file header including projection definitions
  meta <- stiltread::read_met_header(path)

  # Determine relevant wind variables for given vertical level
  if (lvl == 0) {
    uvar <- 'U10M'
    vvar <- 'V10M'
  } else {
    uvar <- 'UWND'
    vvar <- 'VWND'
  }

  # Read grid relative winds
  u <- read_met(path, uvar, yy, mm, dd, hh, lvl, wnd_warning = F)
  v <- read_met(path, vvar, yy, mm, dd, hh, lvl, wnd_warning = F)

  # Project winds onto latlon grid using bilinear interpolation
  u <- raster::projectRaster(u, crs = '+proj=longlat')
  v <- raster::projectRaster(v, crs = '+proj=longlat')

  # Use WRF python module to calculate earth relative winds
  wrf <- reticulate::import('wrf', delay_load = T)
  uv <- wrf$uvmet(
    u = matrix(raster::values(u),
               nrow = dim(u)[2],
               ncol = dim(u)[1],
               byrow = F),
    v = matrix(raster::values(v),
               nrow = dim(u)[2],
               ncol = dim(u)[1],
               byrow = F),
    lat = matrix(raster::coordinates(u)[, 2],
                 nrow = dim(u)[2],
                 ncol = dim(u)[1],
                 byrow = F),
    lon = matrix(raster::coordinates(u)[, 1],
                 nrow = dim(u)[2],
                 ncol = dim(u)[1],
                 byrow = F),
    cen_long = meta$ref_lon,
    cone = sin(abs(meta$ref_lat) * pi/180),
    meta = F
  )


  raster::stack(list(u = raster(uv[1, , ], template = u),
             v = raster(uv[2, , ], template = v)))
}
