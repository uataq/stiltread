# stiltread R package

This package provides tools for interfacing with input and output files used by the [Stochastic Time Inverted Lagrangian Transport (STILT)](http://uataq.github.io/stilt) model. 


## Installation

Package can be installed using `devtools`.

```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('uataq/stiltread')
```

Then, load the package and install the dependencies.

```r
library(stiltread)
install_stiltread()
```

To read a raster layer from a HRRR data file,

```r
library(stiltread)
shgt <- read_met('/path/to/arl/20150617.00z.hrrra', 
                 var = 'shgt',
                 yy = 15,
                 mm = 6,
                 dd = 17,
                 hh = 3,
                 lvl = 0)
shgt
# class       : RasterLayer 
# dimensions  : 1059, 1799, 1905141  (nrow, ncol, ncell)
# resolution  : 3005.169, 2991.952  (x, y)
# extent      : -2700991, 2705308, -1580359, 1588118  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=lcc +lon_0=-97.5 +lat_0=38.5 +lat_1=38.5 +lat_2=38.5 +ellps=WGS84 +no_defs
# data source : in memory
# names       : layer 
# values      : -80.036, 4175.964  (min, max)
```

which returns a `raster::RasterLayer` containing data, grid coordinates, and projection information using the standard [PROJ4](https://proj4.org/) format.


## Wind transformations

Since wind is grid relative, use `read_met_wind` instead of `read_met` to access wind fields. This is called in the same way as `read_met`, only no `var` argument is required. This returns a `rasterStack` with `u` and `v` layers that have been rotated into `+proj=longlat`.

```{r}
library(stiltread)
uv <- read_met_wind('/path/to/arl/20150617.00z.hrrra', 
                    yy = 15,
                    mm = 6,
                    dd = 17,
                    hh = 3,
                    lvl = 0)
uv
# class       : RasterStack 
# dimensions  : 72, 78, 5616, 2  (nrow, ncol, ncell, nlayers)
# resolution  : 0.0345, 0.0261  (x, y)
# extent      : -113.4537, -110.7627, 39.81678, 41.69598  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +ellps=WGS84 
# names       :          u,          v 
# min values  :  -6.965451, -10.460653 
# max values  :   5.198425,   5.289214
```

The `u` and `v` layers can be accessed by name using

```{r}
uv$u
# class       : RasterLayer 
# dimensions  : 72, 78, 5616  (nrow, ncol, ncell)
# resolution  : 0.0345, 0.0261  (x, y)
# extent      : -113.4537, -110.7627, 39.81678, 41.69598  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +ellps=WGS84 
# data source : in memory
# names       : u 
# values      : -6.965451, 5.198425  (min, max)
```


## Visualizations

Since `read_met` returns a `raster::RasterLayer` with projection metadata, we can leverage standard mapping libraries in R.

```r
plot(shgt, main = 'Elevation')
```

<p align="center">
  <img src="man/figures/shgt-example.png" width=500 />
</p>

For an interactive leaflet map, 

```r
library(leaflet)
leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addRasterImage(shgt, opacity = 0.5, colors = viridis::viridis(16))
```
<p align="center">
  <img src="man/figures/shgt-example-leaflet.png" width=500 />
</p>


## Limitations

1. Returned raster layers are limited to files that use Lambert Conformal Conic for the map projection, which is typical for midlatitudes.
1. Upper levels of pressure variables are often recorded as a difference from the base level. This calculation needs to be performed by the user.
