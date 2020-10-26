library(mobloc)
library(magick)
library(grid)
library(tmap)
library(tmaptools)
library(stars)



############ mobloc #################################


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

ZL_comp_prior <- create_prior(ZL_network_prior, ZL_landuse_prior, weights = c(.5, .5))

if (FALSE) {
    explore_mobloc(ZL_cellplan, ZL_raster, ZL_strength,
                   list(landuse = ZL_landuse_prior, network = ZL_network_prior, uniform = ZL_uniform_prior),
                   list(Strength = ZL_strength_llh, Voronoi = ZL_voronoi_llh),
                   param = ZL_param)
}


priors = list(uniform = ZL_uniform_prior, landuse = ZL_landuse_prior, network = ZL_network_prior, comp = ZL_comp_prior)
llhs = list(voronoi = ZL_voronoi_llh, strength = ZL_strength_llh)

############ mobvis  settings and preprocessing #################################



settings = mobvis_settings(style = "cont", cell_offset = 150, cell_size = 3, cell_lwd = 1/2,
                           cell_colors = c("Selected" = "red", "Small cell" = "goldenrod3", "Normal cell" = "gold"),
                           cell_border_col = "black",
                           cell_connection_col = "grey50",
                           palettes = list(
                               dBm = "YlGnBu",
                               s = "YlGnBu",
                               bsm = "ColorBrewer",
                               pg = "YlGnBu",
                               pag = "YlGn",
                               pga = "YlOrBr"
                           ))
settings$cell_colors = c("Selected" = "red", "Small cell" = "white", "Normal cell" = "white")
settings$cell_size = 2.5
settings$cell_lwd = 1



settings2 = settings
settings2$cell_colors = c("Selected" = "red", "Small cell" = "white", "Normal cell" = "black")
settings2$cell_size = 2.5
settings2$cell_lwd = 1
settings2$palettes$bsm = "ColorBrewer29" # 29 stands for a seed to randomize colors; see code below used to pick one

settings2$use_classes = TRUE
settings2$prob_th = NA

outdir = "output/"
cropdir = "output/crop/"


osm = tmaptools::read_osm(x = bb(ZL_raster), zoom = 12)

osm_cr = osm[,300:700,1050:1450]
osm2 = st_transform(osm_cr, crs = st_crs(ZL_raster))


crop_image = function(f, f2) {
    image = image_read(f)
    image2 = image_crop(image, "700x700+930+2500")
    image_write(image2, path = f2)
}

crop_image2 = function(f, f2) {
    image = image_read(f)
    image2 = image_crop(image, "700x510+930+2612")
    image_write(image2, path = f2)
}

save_maps = function(tm, name) {
    f = paste0(outdir, name, ".png")
    f2 = paste0(cropdir, name, ".png")
    tmap::tmap_save(tm, filename = f, width = 4000, height = 4000)
    crop_image(f, f2)
}

save_maps2 = function(tm, name) {
    f = paste0(outdir, name, ".png")
    f2 = paste0(cropdir, name, ".png")
    tmap::tmap_save(tm, filename = f, width = 4000, height = 4000)
    crop_image2(f, f2)
}


############ mobvis  settings and preprocessing #################################


# tm = qtm(ZL_raster) +
#     tm_shape(osm2, raster.downsample = FALSE, raster.warp = FALSE) + tm_rgb() +
#     base_tmap(ZL_cellplan, settings = settings, cells = "EIJ_769_N1") +
#     tm_layout(legend.outside = TRUE, frame = FALSE, legend.show = FALSE, outer.margins = c(.04))
# f = paste0(outdir, "basemap_sel.png")
# f2 = paste0(cropdir, "basemap_sel.png")
# tmap::tmap_save(tm, filename = f, width = 4000, height = 4000)
# crop_image(f, f2)


#map_best_server(ZL_raster, ZL_strength_llh, cp = ZL_cellplan, interactive = T, settings = settings2, title = "") + tm_layout(legend.show = FALSE, scale = 1)

tm = qtm(ZL_raster) +
    tm_shape(osm2, raster.downsample = FALSE, raster.warp = FALSE) + tm_rgb() +
    base_tmap(ZL_cellplan, settings = settings2) +
    tm_layout(legend.outside = TRUE, frame = FALSE, legend.show = FALSE, outer.margins = c(.04))
save_maps2(tm, "basemap")
tm = qtm(ZL_raster) +
    tm_shape(osm2, raster.downsample = FALSE, raster.warp = FALSE) + tm_rgb() +
    base_tmap(ZL_cellplan, settings = settings2, cells = "EIJ_769_N1") +
    tm_layout(legend.outside = TRUE, frame = FALSE, legend.show = FALSE, outer.margins = c(.04))
save_maps2(tm, "basemap2")

############# Figure 1

tm = map_best_server(ZL_raster, ZL_strength_llh, cp = ZL_cellplan, interactive = FALSE, settings = settings2, title = "") + tm_layout(legend.show = FALSE, scale = 1)
save_maps2(tm, "bsm_ss")

tm = map_best_server(ZL_raster, ZL_voronoi_llh, cp = ZL_cellplan, interactive = FALSE, settings = settings2, title = "") + tm_layout(legend.show = FALSE, scale = 1)
save_maps2(tm, "bsm_v")

tm = map_sig_strength(ZL_raster, ZL_strength, cp = ZL_cellplan, cells = "EIJ_769_N1", interactive = FALSE, settings = settings2, title = "", type = "s") + tm_layout(legend.show = FALSE, scale = 1)
save_maps2(tm, "ss_s")

tm = map_sig_strength(ZL_raster, ZL_strength, cp = ZL_cellplan, cells = "EIJ_769_N1", interactive = FALSE, settings = settings2, title = "", type = "dBm") + tm_layout(legend.show = FALSE, scale = 1)
save_maps2(tm, "ss_dBm")

layout_legend = tm_layout(legend.only = TRUE, legend.outside = FALSE, scale = .95, bg.color = "white", frame = FALSE, inner.margins = 0, outer.margins = 0)
tm = map_sig_strength(ZL_raster, ZL_strength, cp = ZL_cellplan, cells = "EIJ_769_N1", interactive = FALSE, settings = settings2, title = "", type = "s") + layout_legend
tmap_save(tm, filename = paste0(cropdir, "ss_s_legend.png"), width = 325, height = 510)

tm = map_sig_strength(ZL_raster, ZL_strength, cp = ZL_cellplan, cells = "EIJ_769_N1", interactive = FALSE, settings = settings2, title = "", type = "dBm") + layout_legend
tmap_save(tm, filename = paste0(cropdir, "ss_dBm_legend.png"), width = 325, height = 510)




#widths = c(10, 700, 50, 700, 50, 700, 10)
widths = c(10, 700, 50, 325, 50, 325, 50, 325, 50, 325, 10)
heights = c(10, 510, 100, 50, 510, 100)

png(paste0(outdir, "mobloc_bsm.png"), width = sum(widths), height = sum(heights))

grid.newpage()

grd = viewport(layout = grid.layout(ncol = length(widths), nrow = length(heights),
                                    widths = widths,
                                    heights = heights,
                                    default.units = "native"), xscale = c(0, sum(widths)), yscale = c(0, sum(heights)),
               gp = gpar(cex = 4.5),
               clip = "off")
pushViewport(grd)

cellplot = function(row, col, width = 1, height = 1, e) {
    pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
    pushViewport(viewport(width = width, height = height))
    e
    upViewport(2)
}
frame_lwd = 6

cellplot(3, 2, e = grid.text("(a) Basemap", just = "left", x = .05))
cellplot(2, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(3, 4:6, e = grid.text("(b) Voronoi", just = "left", x = .05))
cellplot(2, 4:6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(3, 8:10, e = grid.text("(c) Best Server Map", just = "left", x = .05))
cellplot(2, 8:10, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(6, 2, e = grid.text("(d) Signal strength", just = "left", x = .05))
cellplot(5, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
#cellplot(5, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(6, 6:8, e = grid.text("(e) Signal dominance", just = "left", x = .05))
cellplot(5, 6:8, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
#cellplot(5, 10, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

dev.off()

image_read(paste0(outdir, "mobloc_bsm.png")) %>%
    image_composite(image_read("output/crop/basemap.png"), offset = "+010+010") %>%
    image_composite(image_read("output/crop/bsm_v.png"), offset = "+760+010") %>%
    image_composite(image_read("output/crop/bsm_ss.png"), offset = "+1510+010") %>%
    image_composite(image_read("output/crop/ss_dBm.png"), offset = "+010+670") %>%
    image_composite(image_read("output/crop/ss_dBm_legend.png"), offset = "+760+670") %>%
    image_composite(image_read("output/crop/ss_s.png"), offset = "+1135+670") %>%
    image_composite(image_read("output/crop/ss_s_legend.png"), offset = "+1885+670") %>%
    image_write(path = paste0(outdir, "mobloc_bsm.png"))






############# Figure 2

mapply(function(prior, prior_name) {
    ## Prior
    tm = map_pg(prior, cp = ZL_cellplan, cells = "EIJ_769_N1", interactive = FALSE, settings = settings) + tm_layout(legend.show = FALSE, scale = 1)
    save_maps2(tm, paste0("prior_", prior_name))

    mapply(function(llh, llh_name) {

        ## Likelihood
        if (prior_name == "uniform") {
            tm = map_pag(rst = ZL_raster, dt = llh, cells = "EIJ_769_N1", cp = ZL_cellplan, interactive = FALSE, settings = settings) + tm_layout(legend.show = FALSE, scale = 1)
            save_maps2(tm, paste0("llh_", llh_name))
        }

        ## Posterior
        post = calculate_posterior(prior, llh, ZL_raster)
        tm = map_pga(rst = ZL_raster, dt = post, cells = "EIJ_769_N1", cp = ZL_cellplan, interactive = FALSE, settings = settings) + tm_layout(legend.show = FALSE, scale = 1)
        save_maps2(tm, paste0("post_", prior_name, "_", llh_name))

    }, llhs, names(llhs), SIMPLIFY = FALSE)

}, priors, names(priors), SIMPLIFY = FALSE)




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





widths = c(100, 700, 100, 700, 50, 700, 10)
#heights = c(10, 100, 700, 100, 50, 700, 50, 700, 50, 700, 100)
heights = c(10, 100, 510, 100, 50, 510, 50, 510, 50, 510, 50, 510, 100)


png(paste0(outdir, "mobloc_multiples.png"), width = sum(widths), height = sum(heights))

grid.newpage()

grd = viewport(layout = grid.layout(ncol = length(widths), nrow = length(heights),
                              widths = widths,
                              heights = heights,
                              default.units = "native"), xscale = c(0, sum(widths)), yscale = c(0, sum(heights)),
               gp = gpar(cex = 4.5),
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

cellplot(4, 2, e = grid.text("(a) Basemap", just = "left", x = .05))
cellplot(3, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))


cellplot(4, 4, e = grid.text("(b) Connection likelihoods", just = "left", x = .05))
cellplot(2, 4, e = grid.text("Voronoi", gp = gpar(cex = cex_small)))
cellplot(2, 6, e = grid.text("Signal strength", gp = gpar(cex = cex_small)))
cellplot(3, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(3, 6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(13, 2, e = grid.text("(c) Location priors", just = "left", x = .05))
cellplot(6, 1, e = grid.text("Uniform", rot = 90, gp = gpar(cex = cex_small)))
cellplot(8, 1, e = grid.text("Land use", rot = 90, gp = gpar(cex = cex_small)))
cellplot(10, 1, e = grid.text("Network", rot = 90, gp = gpar(cex = cex_small)))
cellplot(12, 1, e = grid.text("Composite", rot = 90, gp = gpar(cex = cex_small)))

cellplot(6, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(8, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(10, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(12, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(13, 4, e = grid.text("(d) Location posteriors", just = "left", x = .05))
cellplot(6, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(8, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(10, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(12, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(6, 6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(8, 6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(10, 6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))
cellplot(12, 6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

grid.lines(y = unit(c(2330, 2330), "native"), gp = gpar(lwd = 10, col = "grey65"))
grid.lines(x = unit(c(850, 850), "native"), gp = gpar(lwd = 10, col = "grey65"))

dev.off()

cell_offset = function(row, col) {
    hs = cumsum(heights)
    ws = cumsum(widths)
    paste0("+", ws[col-1], "+", hs[row-1])
}

gc()
image_read(paste0(outdir, "mobloc_multiples.png")) %>%
    image_composite(image_read("output/crop/basemap2.png"), offset = cell_offset(3,2)) %>%

    image_composite(image_read("output/crop/llh_voronoi.png"), offset = cell_offset(3,4)) %>%
    image_composite(image_read("output/crop/llh_strength.png"), offset = cell_offset(3,6)) %>%

    image_composite(image_read("output/crop/prior_uniform.png"), offset = cell_offset(6,2)) %>%
    image_composite(image_read("output/crop/post_uniform_voronoi.png"), offset = cell_offset(6,4)) %>%
    image_composite(image_read("output/crop/post_uniform_strength.png"), offset = cell_offset(6,6)) %>%

    image_composite(image_read("output/crop/prior_landuse.png"), offset = cell_offset(8,2)) %>%
    image_composite(image_read("output/crop/post_landuse_voronoi.png"), offset = cell_offset(8,4)) %>%
    image_composite(image_read("output/crop/post_landuse_strength.png"), offset = cell_offset(8,6)) %>%

    image_composite(image_read("output/crop/prior_network.png"), offset = cell_offset(10,2)) %>%
    image_composite(image_read("output/crop/post_network_voronoi.png"), offset = cell_offset(10,4)) %>%
    image_composite(image_read("output/crop/post_network_strength.png"), offset = cell_offset(10,6)) %>%

    image_composite(image_read("output/crop/prior_comp.png"), offset = cell_offset(12,2)) %>%
    image_composite(image_read("output/crop/post_comp_voronoi.png"), offset = cell_offset(12,4)) %>%
    image_composite(image_read("output/crop/post_comp_strength.png"), offset = cell_offset(12,6)) %>%

    image_write(path = paste0(outdir, "mobloc_multiples.png"))


############# Figure 3 (TA)
ta_bands = c(15, 25, 35)

settings3 = settings2
settings3$prob_th = 0

mapply(function(prior, prior_name) {
    mapply(function(llh, llh_name) {

        ## Likelihood

        ## Posterior
        post = calculate_posterior(prior, llh, ZL_raster)
        post_ta = update_posterior_TA(post, raster = ZL_raster, cp = ZL_cellplan, param = ZL_param, elev = ZL_elevation)

        lapply(ta_bands, function(ta) {
            post_ta_sel = post_ta[post_ta$TA == ta]
            #browser()
            tm = qtm(ZL_raster) +
                tm_shape(osm2, raster.downsample = FALSE, raster.warp = FALSE) + tm_rgb() +
                #base_tmap(ZL_cellplan, settings = settings2)

                map_pga(rst = ZL_raster, dt = post_ta_sel, cells = "EIJ_769_N1", cp = ZL_cellplan, interactive = FALSE, settings = settings3) + tm_layout(legend.show = FALSE, scale = 1)
            save_maps2(tm, paste0("post_", prior_name, "_", llh_name, "_TA", ta))

        })
    }, llhs[2], names(llhs)[2], SIMPLIFY = FALSE)

}, priors[4], names(priors)[4], SIMPLIFY = FALSE)





#widths = c(10, 700, 50, 700, 50, 700, 10)
widths = c(10, 700, 50, 700, 50, 700, 10)
heights = c(10, 510, 100)

png(paste0(outdir, "mobloc_ta.png"), width = sum(widths), height = sum(heights))

grid.newpage()

grd = viewport(layout = grid.layout(ncol = length(widths), nrow = length(heights),
                                    widths = widths,
                                    heights = heights,
                                    default.units = "native"), xscale = c(0, sum(widths)), yscale = c(0, sum(heights)),
               gp = gpar(cex = 4.5),
               clip = "off")
pushViewport(grd)

cellplot = function(row, col, width = 1, height = 1, e) {
    pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
    pushViewport(viewport(width = width, height = height))
    e
    upViewport(2)
}
frame_lwd = 6

cellplot(3, 2, e = grid.text("(a) TA value 15", just = "left", x = .05))
cellplot(2, 2, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(3, 4, e = grid.text("(b) TA value 25", just = "left", x = .05))
cellplot(2, 4, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

cellplot(3, 6, e = grid.text("(c) TA value 35", just = "left", x = .05))
cellplot(2, 6, e = grid.rect(gp=gpar(fill = "grey80", lwd = frame_lwd)))

dev.off()

image_read(paste0(outdir, "mobloc_ta.png")) %>%
    image_composite(image_read("output/crop/post_comp_strength_TA15.png"), offset = "+010+010") %>%
    image_composite(image_read("output/crop/post_comp_strength_TA25.png"), offset = "+760+010") %>%
    image_composite(image_read("output/crop/post_comp_strength_TA35.png"), offset = "+1510+010") %>%
    image_write(path = paste0(outdir, "mobloc_ta.png"))




