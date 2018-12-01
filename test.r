library(leaflet)
library(raster)
library(stiltread)

r <- read_met('../met_conus.hrrra', 'shgt')

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addRasterImage(r,
                 colors = viridis::viridis(16),
                 opacity = 0.75)
