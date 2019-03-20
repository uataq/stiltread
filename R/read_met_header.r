#' Read ARL packed meteorological data file header
#' @author Ben Fasoli
#'
#' Extracts metadata from the meteorological data file header
#'
#' @param path to ARL packed file
#'
#' @export

read_met_header <- function(path) {

  if (!file.exists(path)) stop('File does not exist')

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
  proj_vals <- gsub('(.{7})', '\\1_', substring(header, 10, 85))
  proj_vals <- as.numeric(unlist(strsplit(proj_vals, '_')))
  names(proj_vals) <- c('pole_lat', 'pole_lon', 'ref_lat', 'ref_lon',
                        'ref_grid', 'orientation', 'cone_angle', 'sync_x',
                        'sync_y', 'sync_lat', 'sync_lon')
  proj_vals <- ifelse(proj_vals > 180, - (360 - proj_vals), proj_vals)

  # Initialize output container for named attributes
  output <- as.list(proj_vals)

  # Extract information about 3d grid
  grid_vals <- gsub('(.{3})', '\\1_', substring(header, 94, 102))
  grid_vals <- as.numeric(unlist(strsplit(grid_vals, '_')))
  output$nx <- as.integer(nx_is_large + grid_vals[1])
  output$ny <- as.integer(ny_is_large + grid_vals[2])
  output$nz <- as.integer(grid_vals[3])


  # Use WRF python module to calculate ll of grid positions
  wrf <- reticulate::import('wrf', delay_load = T)
  latlon_domain <- wrf$xy_to_ll_proj(
    x=c(0, output$nx - 1),
    y=c(0, output$ny - 1),
    squeeze=T,
    meta=F,
    map_proj=1,
    truelat1=output$ref_lat,
    truelat2=output$ref_lat,
    stand_lon=output$ref_lon,
    ref_lat=output$sync_lat,
    ref_lon=output$sync_lon,
    known_x=0,
    known_y=0,
    pole_lat=output$pole_lat,
    pole_lon=output$pole_lon,
    dx=output$ref_grid * 1000,
    dy=output$ref_grid * 1000
  )

  output$crs <- glue::glue('+proj=lcc ',
                           '+lon_0={ref_lon} ',
                           '+lat_0={cone_angle} ',
                           '+lat_1={cone_angle} ',
                           '+lat_2={cone_angle} ',
                           '+ellps=WGS84 ',
                           '+no_defs ',
                           .envir = output)

  crs_domain <- rgdal::rawTransform('+proj=longlat',
                                    output$crs,
                                    n = as.integer(2),
                                    x = range(latlon_domain[2, ]),
                                    y = range(latlon_domain[1, ]))
  output$crs_domain <- as.numeric(unlist(crs_domain)[1:4])

  output
}
