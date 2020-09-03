\dontrun{

require(mobloc)

# set parameters
ZL_param <- mobloc_param()

# load data
data("ZL_cellplan", "ZL_muni", "ZL_elevation", "ZL_landuse")

# create environment layer (needed to calculate path loss exponent (ple))
ZL_envir <- combine_raster_layers(ZL_landuse, weights = c(1, 1, 1, 0, 0))

# validate cellplan
ZL_cellplan <- validate_cellplan(ZL_cellplan, param = ZL_param, region = ZL_muni,
    envir = ZL_envir, elevation = ZL_elevation)

# create raster
ZL_bbox <- sf::st_bbox(c(xmin = 4012000, ymin = 3077000, xmax = 4048000, ymax = 3117000),
    crs = sf::st_crs(3035))
ZL_raster <- create_raster(ZL_bbox)

# compute the signal strength model
ZL_strength <- compute_sig_strength(cp = ZL_cellplan, raster = ZL_raster,
    elevation = ZL_elevation, param = ZL_param)

# create likelihoods
ZL_strength_llh <- create_strength_llh(ZL_strength, param = ZL_param)
ZL_voronoi_llh <- create_voronoi_llh(ZL_cellplan, ZL_raster)

# create priors
ZL_uniform_prior <- create_uniform_prior(ZL_raster)
ZL_network_prior <- create_network_prior(ZL_strength, ZL_raster)
ZL_landuse_prior <- create_prior(ZL_landuse, weights = c(1, 1, .1, 0, .5))

# explore the results
explore_mobloc(ZL_cellplan, ZL_raster, ZL_strength,
               list(landuse = ZL_landuse_prior, network = ZL_network_prior, uniform = ZL_uniform_prior),
               list(Strength = ZL_strength_llh, Voronoi = ZL_voronoi_llh),
               param = ZL_param)




# prior
cp = ZL_cellplan
rst = raster::trim(ZL_network_prior)

dt = ZL_strength[cell=="SIT_440_N3", ]

rst = create_p_raster(ZL_raster, dt, "s")

map_mob_cells(cp, rst, title = "Test", interactive = FALSE)



# mobloc
map_sig_strength(rst = ZL_raster, dt = ZL_strength, cp = ZL_cellplan, cells = c("BEE_150_N1", "BEE_150_N2"), region = ZL_muni, type = "dBm")



map_sig_strength <- function(rst, dt, cp, cells = NA, region = NULL, type = c("dBm", "s"), interactive = TRUE) {
    if (is.na(cells[1])) cells <- dt$cell

    dtsel <- dt[cell %in% cells, ][, list(dBm = max(dBm), s = max(s)), by = rid]


    p = create_p_raster(rst, dtsel, type = type)
    map_mob_cells(cp, p, var = type, borders = region, interactive = interactive)
}




cp = ZL_cellplan
rst = raster::trim(ZL_network_prior)

dt = ZL_strength[cell=="SIT_440_N3", ]

rst = create_p_raster(ZL_raster, dt, "s")

map_mob_cells(cp, rst, title = "Test", interactive = FALSE)


create_p_raster()


map_mob_cells(cp, rst, title = "Test")



}
