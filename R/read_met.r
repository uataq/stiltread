#' Read ARL packed meteorological data file layer
#' @author Ben Fasoli
#'
#' Extracts the given variable from file and returns a raster with associated
#' projection metadata
#'
#' @param path to ARL packed file
#' @param var four digit character code of variable. Examples include SHGT PRSS
#'   MSLP TPP1 DIFR T02M DP2M PBLH DSWF ULWF U10M V10M VSBY CSNO CICE CFZR CRAI
#'   LIB4 CAPE CINH TCLD REFC CLDB CLDT
#'
#' @useDynLib stiltread
#'
#' @import glue raster reticulate rgdal
#' @export

read_met <- function(path, var) {

  if (!file.exists(path)) stop('File does not exist')
  var <- toupper(var)

  # Read fixed width ascii record containing first label and file header
  meta <- readChar(path, 166)

  # ARL files only allow three characters for grid size (nx, ny). For grids with
  # over 1,000 points, the thousands digit is specified by a letter prior to the
  # index label. A: 1000, B: 2000, C: 3000, etc.
  nx_is_large <- 1000 * base::match(substr(meta, 13, 13), LETTERS, nomatch = 0)
  ny_is_large <- 1000 * base::match(substr(meta, 14, 14), LETTERS, nomatch = 0)

  # Header contains projection and grid size information
  header <- substring(meta, 51)

  # Extract named projection information
  proj_vals <- gsub('(.{7})', '\\1_', substring(header, 10, 93))
  proj_vals <- as.numeric(unlist(strsplit(proj_vals, '_')))
  names(proj_vals) <- c('pole_lat', 'pole_lon', 'ref_lat', 'ref_lon',
                        'ref_grid', 'orientation', 'cone_angle', 'sync_x',
                        'sync_y', 'sync_lat', 'sync_lon', 'dummy')
  proj_vals <- ifelse(proj_vals > 180, - (360 - proj_vals), proj_vals)
  proj_vals <- as.list(proj_vals)

  # Extract information about 3d grid
  grid_vals <- gsub('(.{3})', '\\1_', substring(header, 94, 102))
  grid_vals <- as.numeric(unlist(strsplit(grid_vals, '_')))
  nx <- as.integer(nx_is_large + grid_vals[1])
  ny <- as.integer(ny_is_large + grid_vals[2])
  nz <- as.integer(grid_vals[3])
  grid <- expand.grid(x = 1:nx, y = 1:ny)

  # Extract desired variable grid
  out <- .Fortran('fetch_grid', PACKAGE = 'stiltread',
                  file = path,
                  var = var,
                  nx = nx,
                  ny = ny,
                  rdata = array(0, dim = c(nx, ny)))

  # Use WRF python module to calculate ll of grid positions
  wrf <- reticulate::import('wrf', delay_load = T)
  latlon <- wrf$xy_to_ll_proj(
    x=c(1, nx),
    y=c(1, ny),
    squeeze=T,
    meta=F,
    map_proj=1, # LCC
    truelat1=proj_vals$ref_lat,
    truelat2=proj_vals$ref_lat,
    stand_lon=proj_vals$ref_lon,
    ref_lat=proj_vals$sync_lat,
    ref_lon=proj_vals$sync_lon,
    known_x=1,
    known_y=1,
    pole_lat=proj_vals$pole_lat,
    pole_lon=proj_vals$pole_lon,
    dx=proj_vals$ref_grid * 1000,
    dy=proj_vals$ref_grid * 1000
  )

  proj_crs <- glue::glue('+proj=lcc ',
                         '+lon_0={ref_lon} ',
                         '+lat_0={cone_angle} ',
                         '+lat_1={cone_angle} ',
                         '+lat_2={cone_angle} ',
                         '+ellps=WGS84 ',
                         '+no_defs ',
                         .envir = proj_vals)

  extent_meters <- rgdal::rawTransform('+proj=longlat',
                                       proj_crs,
                                       n = as.integer(2),
                                       x = range(latlon[2, ]),
                                       y = range(latlon[1, ]))
  extent_meters <- as.numeric(unlist(extent_meters)[1:4])

  raster::raster(apply(t(out$rdata), 2, rev),
                 crs = as.character(proj_crs),
                 xmn = extent_meters[1],
                 xmx = extent_meters[2],
                 ymn = extent_meters[3],
                 ymx = extent_meters[4]
  )
}
