# library(sf)
# library(tmap)
# library(raster)
# library(stars)
#
# map = st_as_sfc(readLines("data/map.wkt"))
#
#
# grd = read.csv("data/output/grid.csv")
#
# rst = raster(nrows = grd$No.Tiles.Y,
#        ncols = grd$No.Tiles.X,
#        xmn = grd$Origin.X,
#        ymn = grd$Origin.Y,
#        xmx = grd$Origin.X + (grd$No.Tiles.X * grd$X.Tile.Dim),
#        ymx = grd$Origin.Y + (grd$No.Tiles.Y * grd$Y.Tile.Dim))
#
# rst[] = 1:length(rst)
#
# rst = raster::flip(rst, "y")
# rst = st_as_stars(rst)
#
#
# st_crs(rst) = 28992
# map = st_set_crs(map, 28992)
#
#
# plot(rst)
# a = read.csv("data/output/antennas.csv")
#
# asf = st_as_sf(a, coords = c("x", "y"), crs = 28992)
#
#
# qtm(rst) + qtm(map) + qtm(asf)
#
