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



settings = mobvis_settings(style = "cont", cell_offset = 150, cell_size = 3, cell_lwd = 1/2,
                           cell_colors = c("Selected" = "red", "Small cell" = "goldenrod3", "Normal cell" = "gold"),
                           palettes = list(
                               dBm = "YlGnBu",
                               s = "YlGnBu",
                               bsm = "ColorBrewer",
                               pg = "YlGnBu",
                               pag = "YlGn",
                               pga = "YlOrBr"
                           ))
#settings$cell_colors = c("Selected" = "red", "Small cell" = "white", "Normal cell" = "black")
settings$cell_size = 3
settings$cell_lwd = 1


explore_mobloc(ZL_cellplan, ZL_raster, ZL_strength,
               list(landuse = ZL_landuse_prior, network = ZL_network_prior, uniform = ZL_uniform_prior),
               list(Strength = ZL_strength_llh, Voronoi = ZL_voronoi_llh),
               param = ZL_param)


priors = list(uniform = ZL_uniform_prior, landuse = ZL_landuse_prior, network = ZL_network_prior)
llhs = list(voronoi = ZL_voronoi_llh, strength = ZL_strength_llh)

outdir = "output/"
cropdir = "output/crop/"

library(magick)

crop_image = function(f, f2) {
    image = image_read(f)
    image2 = image_crop(image, "700x700+930+2500")
    image_write(image2, path = f2)
}

mapply(function(prior, prior_name) {
    ## Prior
    tm = map_pg(prior, cp = ZL_cellplan, cells = "EIJ_769_N1", interactive = FALSE, settings = settings) + tm_layout(legend.show = FALSE, scale = 1)
    f = paste0(outdir, "prior_", prior_name, ".png")
    f2 = paste0(cropdir, "prior_", prior_name, ".png")
    tmap::tmap_save(tm, filename = f, width = 4000, height = 4000)
    crop_image(f, f2)

    mapply(function(llh, llh_name) {

        ## Likelihood
        if (prior_name == "uniform") {
            tm = map_pag(rst = ZL_raster, dt = llh, cells = "EIJ_769_N1", cp = ZL_cellplan, interactive = FALSE, settings = settings) + tm_layout(legend.show = FALSE, scale = 1)
            f = paste0(outdir, "llh_", llh_name, ".png")
            f2 = paste0(cropdir, "llh_", llh_name, ".png")
            tmap::tmap_save(tm, filename = f, width = 4000, height = 4000)
            crop_image(f, f2)
        }

        ## Posterior
        post = calculate_posterior(prior, llh, ZL_raster)
        tm = map_pga(rst = ZL_raster, dt = post, cells = "EIJ_769_N1", cp = ZL_cellplan, interactive = FALSE, settings = settings) + tm_layout(legend.show = FALSE, scale = 1)
        f = paste0(outdir, "post_", prior_name, "_", llh_name, ".png")
        f2 = paste0(cropdir, "post_", prior_name, "_", llh_name, ".png")
        tmap::tmap_save(tm, filename = f, width = 4000, height = 4000)
        crop_image(f, f2)


    }, llhs, names(llhs), SIMPLIFY = FALSE)

}, priors, names(priors), SIMPLIFY = FALSE)


map_pg(ZL_landuse_prior, cp = ZL_cellplan, cells = "EIJ_769_N1", settings = settings, interactive = FALSE)

library(tmaptools)



osm = tmaptools::read_osm(x = bb(ZL_raster), zoom = 12)

library(stars)

#osm2 = st_warp(osm, crs = st_crs(ZL_raster))
osm_cr = osm[,300:700,1050:1450]
osm2 = st_transform(osm_cr, crs = st_crs(ZL_raster))





## Basemap
tm = qtm(ZL_raster) +
    tm_shape(osm2, raster.downsample = FALSE, raster.warp = FALSE) + tm_rgb() +
    base_tmap(ZL_cellplan, settings = settings, cells = "EIJ_769_N1") +
tm_layout(legend.outside = TRUE, frame = FALSE, legend.show = FALSE, outer.margins = c(.04))
f = paste0(outdir, "basemap_sel.png")
f2 = paste0(cropdir, "basemap_sel.png")
tmap::tmap_save(tm, filename = f, width = 4000, height = 4000)
crop_image(f, f2)



settings2 = settings
settings2$cell_colors = c("Selected" = "red", "Small cell" = "white", "Normal cell" = "black")
settings2$cell_size = 2.5
settings2$cell_lwd = 1

# finding best color palette
# for (k in 21:50) {
#     settings2$palettes$bsm = paste0("ColorBrewer", k)
#
#     tm = map_best_server(ZL_raster, ZL_strength_llh, cp = ZL_cellplan, interactive = FALSE, settings = settings2, title = "") + tm_layout(legend.show = FALSE, scale = 1)
#     f = paste0(outdir, "bsm_ss", k, ".png")
#     f2 = paste0(cropdir, "bsm_ss", k, ".png")
#     tmap::tmap_save(tm, filename = f, width = 4000, height = 4000)
#     crop_image(f, f2)
# }

settings2$palettes$bsm = "ColorBrewer29"
#map_best_server(ZL_raster, ZL_strength_llh, cp = ZL_cellplan, interactive = T, settings = settings2, title = "") + tm_layout(legend.show = FALSE, scale = 1)

## Basemap
tm = qtm(ZL_raster) +
    tm_shape(osm2, raster.downsample = FALSE, raster.warp = FALSE) + tm_rgb() +
    base_tmap(ZL_cellplan, settings = settings2) +
    tm_layout(legend.outside = TRUE, frame = FALSE, legend.show = FALSE, outer.margins = c(.04))
f = paste0(outdir, "basemap.png")
f2 = paste0(cropdir, "basemap.png")
tmap::tmap_save(tm, filename = f, width = 4000, height = 4000)
crop_image(f, f2)



tm = map_best_server(ZL_raster, ZL_strength_llh, cp = ZL_cellplan, interactive = FALSE, settings = settings2, title = "") + tm_layout(legend.show = FALSE, scale = 1)
f = paste0(outdir, "bsm_ss.png")
f2 = paste0(cropdir, "bsm_ss.png")
tmap::tmap_save(tm, filename = f, width = 4000, height = 4000)
crop_image(f, f2)



tm = map_best_server(ZL_raster, ZL_voronoi_llh, cp = ZL_cellplan, interactive = FALSE, settings = settings2, title = "") + tm_layout(legend.show = FALSE, scale = 1)
f = paste0(outdir, "bsm_v.png")
f2 = paste0(cropdir, "bsm_v.png")
tmap::tmap_save(tm, filename = f, width = 4000, height = 4000)
crop_image(f, f2)





# enhance_image = function(f, f2) {
#     image = image_read(f)
#     image2 = image_normalize(image)
#     image_write(image2, path = f2)
# }
#
# f2 = paste0(cropdir, "basemap.png")
# f3 = paste0(cropdir, "basemap_mod.png")
#
# enhance_image(f2, f3)




library(grid)

widths = c(100, 700, 100, 700, 50, 700, 10)
heights = c(10, 100, 700, 100, 50, 700, 50, 700, 50, 700, 100)


png("mobloc_multiples.png", width = sum(widths), height = sum(heights))

grid.newpage()

grd = viewport(layout = grid.layout(ncol = length(widths), nrow = length(heights),
                              widths = widths,
                              heights = heights,
                              default.units = "native"), xscale = c(0, sum(widths)), yscale = c(0, sum(heights)),
               gp = gpar(cex = 5),
               clip = "off")
pushViewport(grd)

cellplot = function(row, col, width = 1, height = 1, e) {
    pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
    pushViewport(viewport(width = width, height = height))
    e
    upViewport(2)
}
cex_small = 0.85
frame_lwd = 6

cellplot(4, 2, e = grid.text("(a) Basemap"))
cellplot(3, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))


cellplot(4, 4:6, e = grid.text("(b) Connection likelihoods"))
cellplot(2, 4, e = grid.text("Voronoi", gp = gpar(cex = cex_small)))
cellplot(2, 6, e = grid.text("Signal strength", gp = gpar(cex = cex_small)))
cellplot(3, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(3, 6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(11, 2, e = grid.text("(c) Location priors"))
cellplot(6, 1, e = grid.text("Uniform", rot = 90, gp = gpar(cex = cex_small)))
cellplot(8, 1, e = grid.text("Network", rot = 90, gp = gpar(cex = cex_small)))
cellplot(10, 1, e = grid.text("Land use", rot = 90, gp = gpar(cex = cex_small)))

cellplot(6, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(8, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(10, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(11, 4:6, e = grid.text("(d) Location posteriors"))
cellplot(6, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(8, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(10, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(6, 6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(8, 6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(10, 6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

#grid.lines(y = unit(c(2300, 2300), "native"), gp = gpar(lwd = 4))
#grid.lines(x = unit(c(850, 850), "native"), gp = gpar(lwd = 4))

dev.off()

image_read("mobloc_multiples.png") %>%
    image_composite(image_read("output/crop/basemap_sel.png"), offset = "+100+110") %>%

    image_composite(image_read("output/crop/llh_voronoi.png"), offset = "+900+110") %>%
    image_composite(image_read("output/crop/llh_strength.png"), offset = "+1650+110") %>%

    image_composite(image_read("output/crop/prior_uniform.png"), offset = "+100+960") %>%
    image_composite(image_read("output/crop/post_uniform_voronoi.png"), offset = "+900+960") %>%
    image_composite(image_read("output/crop/post_uniform_strength.png"), offset = "+1650+960") %>%

    image_composite(image_read("output/crop/prior_landuse.png"), offset = "+100+2460") %>%
    image_composite(image_read("output/crop/post_landuse_voronoi.png"), offset = "+900+2460") %>%
    image_composite(image_read("output/crop/post_landuse_strength.png"), offset = "+1650+2460") %>%

    image_composite(image_read("output/crop/prior_network.png"), offset = "+100+1710") %>%
    image_composite(image_read("output/crop/post_network_voronoi.png"), offset = "+900+1710") %>%
    image_composite(image_read("output/crop/post_network_strength.png"), offset = "+1650+1710") %>%
    image_write(path = "mobloc_multiples.png")





widths = c(10, 700, 50, 700, 50, 700, 10)
heights = c(10, 700, 100)


png("mobloc_bsm.png", width = sum(widths), height = sum(heights))

grid.newpage()

grd = viewport(layout = grid.layout(ncol = length(widths), nrow = length(heights),
                                    widths = widths,
                                    heights = heights,
                                    default.units = "native"), xscale = c(0, sum(widths)), yscale = c(0, sum(heights)),
               gp = gpar(cex = 5),
               clip = "off")
pushViewport(grd)

cellplot = function(row, col, width = 1, height = 1, e) {
    pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
    pushViewport(viewport(width = width, height = height))
    e
    upViewport(2)
}
frame_lwd = 6

cellplot(3, 2, e = grid.text("(a) Basemap"))
cellplot(2, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(3, 4, e = grid.text("(b) Voronoi"))
cellplot(2, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(3, 6, e = grid.text("(c) Best Server Map"))
cellplot(2, 6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))



#grid.lines(y = unit(c(2300, 2300), "native"), gp = gpar(lwd = 4))
#grid.lines(x = unit(c(850, 850), "native"), gp = gpar(lwd = 4))

dev.off()

image_read("mobloc_bsm.png") %>%
    image_composite(image_read("output/crop/basemap.png"), offset = "+010+010") %>%
    image_composite(image_read("output/crop/bsm_v.png"), offset = "+760+010") %>%
    image_composite(image_read("output/crop/bsm_ss.png"), offset = "+1510+010") %>%
    image_write(path = "mobloc_bsm.png")


