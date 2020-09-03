create_connection_lines <- function(cp1, cp2) {
    c1 <- st_coordinates(cp1)
    c2 <- st_coordinates(cp2)

    st_sf(geometry = do.call(st_sfc, lapply(1:nrow(c1), function(i) {
        co <- rbind(c1[i,],
                    c2[i,])
        st_linestring(co)
    })), cell = cp1$cell, crs = st_crs(cp1))
}

get_leafletCRS <- function(epsg) {
    if (epsg == 3035) {
        leafletCRS(crsClass = "L.Proj.CRS",
                   code='EPSG:3035',
                   proj4def="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                   resolutions = 2^(7:0))
    } else {
        leafletCRS(crsClass = "L.CRS.EPSG3857")
    }
}

get_epsg_tiles <- function(map, epsg) {
    if (epsg == 3035) {
        addWMSTiles(map, "https://image.discomap.eea.europa.eu/arcgis/services/GioLandPublic/DEM/MapServer/WmsServer", layers = "Image")
    } else {
        addTiles(map)
    }
}


base_map <- function(cp, offset, epsg) {
    cp2 <- move_cells_into_prop_direction(cp, offset)
    cp_lines <- create_connection_lines(cp, cp2)

    lf <- leaflet(options = leafletOptions(crs = get_leafletCRS(epsg))) %>%
        addPolylines(data = cp_lines %>% st_transform(crs = 4326), color = "#777777", opacity = 1, weight = 3, group = "Cell locations") %>%
        get_epsg_tiles(epsg)

}

base_tmap <- function(cp, offset = 0, borders = NULL, basemaps = "OpenStreetMap", cells = character()) {
    cp$sel = factor(ifelse(cp$cell %in% cells, "Selected",
                    ifelse(cp$small, "Small cell", "Normal cell")), levels = c("Selected", "Small cell", "Normal cell"))
    cp = cp[, c("sel", "small", "direction", "x", "y", "cell")]

    cp2 <- move_cells_into_prop_direction(cp, offset)
    cp_lines <- create_connection_lines(cp, cp2)

    if (is.na(basemaps[1])) {
        # not working yet
#        tmap_options(projection = 0, basemaps = NULL)
        tm <- list()
    } else {
        tm <- tm_basemap("OpenStreetMap")
    }

    cell_palette = c("Selected" = "red", "Small cell" = "gray", "Normal cell" = "gold")

    if (offset > 0) {
        tm <- tm + tm_shape(cp_lines) +
            tm_lines(col = "#777777", lwd = 3, group = "Cell locations", interactive = FALSE) +
            tm_shape(cp2) +
            tm_dots("sel", palette = cell_palette, size = .04, border.col = "black", group = "Cell locations", title = "Cell locations", interactive = TRUE, id = "cell", popup.vars = FALSE, drop.levels = TRUE)
    } else {
        tm <- tm + tm_shape(cp) +
            tm_dots("sel", palette = cell_palette, size = .04, border.col = "black", group = "Cell locations", title = "Cell locations", interactive = TRUE, id = "cell", popup.vars = FALSE, drop.levels = TRUE)
    }

    if (!is.null(borders)) {
        #tm <- tm + tm_shape(borders) + tm_polygons(col = NA, border.col = "black", group = "Cell locations", interactive = FALSE, alpha = 0)
        tm <- tm + tm_shape(borders) + tm_borders(col = "black", group = "Cell locations")
    }
    tm
}



#' Visualize mobile phone cells and auxiliary data
#'
#' Visualize mobile phone cells and auxiliary data, such as signal strength, location estimation variables. This function is used in \code{\link{explore_mobloc}}.
#'
#' @param cp cellplan
#' @param rst raster data
#' @param var if specified, \code{title} and \code{palette} are specified accordingly. Possible values: \code{"dBm", "s", "pga", "pag", "pg", "bsm"}.
#' @param title title
#' @param palette palette The default depends on \code{var}: \code{-Blues} (from ColorBrewer) for unspecified \code{var}, \code{-Greens} for \code{"pag"}, \code{-Blues} for \code{"pg"}, \code{viridis} for \code{"pga"}, \code{Set2} for \code{"bsm"}.
#' @param cells cells to select
#' @param offset offset of the cells. If not 0, the cells are moved into the propagation direction
#' @param borders borders (polygon) of the region of interest
#' @param cell_colors colors of the cells
#' @param interactive should the map be interactive or static?
#' @param basemaps basemaps used in the interactive map
#' @param opacity the opacity of the raster layer.
#' @param proxy should the map be updated in a Shiny app?
#' @import tmap
#' @export
map_mob_cells = function(cp, rst, var = NULL, title = NA, palette = NA, cells = character(), offset = 0, borders = NULL, cell_colors = c("Selected" = "red", "Small cell" = "gray", "Normal cell" = "gold"), interactive = TRUE, basemaps = "OpenStreetMap", opacity = 1, proxy = FALSE) {
    # check required columns
    if (!all(c("cell", "small", "direction", "x", "y") %in% names(cp))) stop("cp does not contain all the required columns: cell, small, direction, x, and y")

    tmap_mode(ifelse(interactive, "view", "plot"))

    # if (proxy) {
    #     tm <- list()
    # } else {
    #     tm <- base_tmap(cp = cp, offset = offset, borders = borders)
    # }
    tm2 <- base_tmap(cp = cp, offset = offset, borders = borders, basemaps = if (proxy) NA else basemaps, cells)



    # create palette for cells
    if (!is.null(names(cell_colors))) {
        cell_palette = cell_colors[match(c("Selected", "Small cell", "Normal cell"), names(cell_colors))]
    } else {
        cell_palette = rep(cell_colors, length.out = 3)
    }
    #pal <- colorFactor(cell_palette, levels = c("Selected", "Small cell", "Normal cell"))


    # check var, title and palette
    if (!is.null(var)) {
        if (!(var %in% c("dBm", "s", "pga", "pag", "pg", "bsm", "none"))) stop("unknown var")
    } else {
        var <- "custom"
    }



    if (var != "none") {
        if (var == "bsm") {
            tmap_options(max.categories = 1000)
        } else if (!is.factor(rst)) {
            values <- rst[]
            maxv <- max(values, na.rm = TRUE)
            minv <- min(values, na.rm = TRUE)
            allOnes <- (minv > .999 && maxv <= 1.001)
            inThousands <- (maxv > 0.01 && maxv < 0.1 && minv >= 0)
            inMillions <- (maxv <= 0.01 && minv >= 0)
            if (allOnes) {
                values2 <- 1
                #values <- pmin(pmax(rst[], 0), 1)
            } else if (inThousands) {
                values2 <- values * 1000
            } else if (inMillions) {
                values2 <- values * 1000000
            } else {
                values2 <- values
            }
            rst[] <- values2
            appendix <- ifelse(allOnes || (!inThousands && !inMillions), "", paste0("<br>(in 1 / ", ifelse(inThousands, "1,000", "1,000,000"), ")"))
        }


        if (is.na(title)) {
            title <- switch(var,
                            dBm = "Signal strength in dBm",
                            s = "Signal dominance - s",
                            bsm = "Best server map",
                            #lu = "Land use prior (in %)",
                            pag = paste0("Connection likelihood - P(a|g)", appendix),
                            pg = paste0("Prior - P(g)", appendix),
                            pga = paste0("Location posterior - P(g|a)", appendix),
                            "")
        }

        if (all(is.na(rst[]))) var <- "empty"

        if (is.na(palette[1])) {
            palette <- ifelse(var %in% c("dBm", "s"), "-Blues",
                              ifelse(var == "pga", "viridis",
                                     ifelse(var == "pag", "-Greens",
                                            ifelse(var == "bsm", "Set2" , "-Blues"))))
        }

        cls <- if (var == "dBm")  {
            dBm_classes
        } else {
            qty_classes
        }

        # if (proxy) {
        #     tm <- list()
        # } else {
        #     tm <- tm_basemap("OpenStreetMap")
        # }
        #
        #     if (offset > 0) {
        #         tm <- tm + tm_shape(cp_lines) +
        #             tm_lines(col = "#777777", lwd = 3, group = "Cell locations") +
        #             tm_shape(cp2) +
        #                 tm_dots("sel", palette = cell_palette, size = .05, border.col = "black", group = "Cell locations")
        #     } else {
        #         tm <- tm + tm_shape(cp) +
        #             tm_dots("sel", palette = cell_palette, size = .05, border.col = "black", group = "Cell locations")
        #     }

        #tm <- tm + tm_shape(borders) + tm_polygons("black", group = "Borders")


        if (var %in% c("dBm", "s")) {
            tm <- tm_shape(rst) +
                tm_raster(names(rst)[1], palette = cls$colors, title = title, breaks = cls$breaks, labels = cls$labels, group = "Data", zindex = 404)
        } else if (var != "empty") {
            tm <- tm_shape(rst) +
                tm_raster(names(rst)[1], palette = palette, n = 7, stretch.palette = FALSE, title = title, group = "Data", zindex = 404)
        }

    } else {
        tm <- list()
        title <- ""
    }


    #tm <- tm + tm2

    #browser()
    if (proxy) {
        tmapProxy("map", x = tm_remove_layer(404) + tm)
    } else {
        tm + tm2 + tm_layout(legend.outside = TRUE)
    }
}



viz_p <- function(cp, rst, var, trans, offset, rect, proxy = FALSE) {
    #browser()

    cp$sel <- factor(ifelse(cp$sel == 2, "Selected", ifelse(cp$small, "Small cell", "Normal cell")), levels = c("Selected", "Small cell", "Normal cell"))

    cp = cp[, c("sel", "small", "direction", "x", "y", "cell")]

    cp2 <- move_cells_into_prop_direction(cp, offset)
    cp_lines <- create_connection_lines(cp, cp2)


    pal <- colorFactor(c("red", "gray70", "gold"), levels = c("Selected", "Small cell", "Normal cell"))

    if (all(is.na(rst[]))) var <- "empty"




    cls <- if (var == "dBm")  {
        dBm_classes
    } else {
        qty_classes
    }

    numpal <- ifelse(var %in% c("dBm", "s"), "Blues",
                     ifelse(var == "pga", "viridis",
                            ifelse(var == "pag", "Greens", "Blues")))

    if (var %in% c("dBm", "s")) {
        pal2 <- colorBin(cls$colors, bins = cls$breaks, na.color = "#00000000")#, dBm_classes$labels)
        #rst2 <- raster::projectRaster(rst, crs = st_crs(4326)$proj4string)
        rst2 <- rst
    } else if (var == "bsm") {
        rst2 <- raster::projectRaster(rst, crs = st_crs(3857)$proj4string, method = "ngb")
        lvls <- raster::levels(rst)[[1]]
        cols <- rep(RColorBrewer::brewer.pal(8, "Set2"), length.out = nrow(lvls))

        bsm_sel <- which(cp$sel == "Selected")

        cols[bsm_sel] <- rep(RColorBrewer::brewer.pal(8, "Dark2"), length.out = nrow(lvls))[bsm_sel]

        if (length(na.omit(unique(rst2[])))==1) {
            cols2 <- cols[bsm_sel]
        } else {
            cols2 <- cols
        }
        #pal2 <- colorFactor(palette = cols, domain = lvls$ID, na.color = "transparent")
    } else if (var != "empty") {
        rst2 <- raster::projectRaster(rst, crs = st_crs(4326)$proj4string, method = "bilinear")
        if (any(is.nan(rst2[]))) {
            rst2 <- raster::projectRaster(rst, crs = st_crs(4326)$proj4string, method = "ngb")
        }

        if (var == "pag") {
            allOnes <- (min(rst2[], na.rm = TRUE) > .9)
            if (allOnes) {
                values <- pmin(pmax(rst2[], 0), 1)
            } else {
                values <- pmin(pmax(rst2[] * 1000, 0), 1000)
            }
        } else {
            values <- pmin(pmax(rst2[] * 1000000, 0), 1000000)
        }

        rst2[] <- values
        rng <- range(values, na.rm = TRUE)

        var_as_discrete <- ((rng[2]- rng[1]) < 1e-9)

        if (var_as_discrete) {
            rng_value <- round(rng[1], 8)
            rst2[!is.na(rst2[])] <- rng_value
            cols2 <- if (numpal == c("viridis")) viridis::viridis(7)[4] else RColorBrewer::brewer.pal(7, numpal)[6]
            labels2 <- format(rng_value)
        } else {
            pal2 <- colorNumeric(palette = numpal, rng, reverse = (numpal != "viridis"),
                                 na.color = "transparent")
        }



    }



    title <- switch(var,
                    dBm = "Signal strength in dBm",
                    s = "Signal dominance - s",
                    bsm = "Best server map",
                    #lu = "Land use prior (in %)",
                    pag = paste0("Connection likelihood - P(a|g)", ifelse(allOnes, "", "<br>(in 1 / 1,000)")),
                    pg = "Prior - P(g)<br>(in 1/1,000,000)",
                    pga = "Location posterior - P(g|a)<br>(in 1/1,000,000)",
                    "Unknown variable") #paste("Prior", pnames[var], " - P(g)<br>(in 1/1,000,000)")


    if (proxy) {
        lf <- leafletProxy("map") %>%
            clearMarkers() %>%
            clearImages() %>%
            clearControls() %>%
            clearShapes()
    } else {
        lf <- leaflet() %>%
            addTiles()
    }


    if (offset > 0) {
        lf <- lf %>%
            addPolylines(data = cp_lines %>% st_transform(crs = 4326), color = "#777777", opacity = 1, weight = 3, group = "Cell locations") %>%
            addCircleMarkers(data = cp2 %>% st_transform(crs = 4326), fillColor = ~pal(sel), color = "black", fillOpacity = 1, radius = 5, weight = 1, group = "Cell locations", layerId = ~cell)
    } else {
        lf <- lf %>%
            addCircleMarkers(data = cp %>% st_transform(crs = 4326), fillColor = ~pal(sel), color = "black", fillOpacity = 1, radius = 5, weight = 1, group = "Cell locations", layerId = ~cell)
    }


    if (var %in% c("dBm", "s")) {
        lf <- lf %>% addRasterImage(x = rst2, opacity = trans, group = title, colors = pal2) %>%
            leaflet::addLayersControl(overlayGroups = c("Cell locations", title), position = "topleft", options = layersControlOptions(collapsed = FALSE)) %>%
            addLegend(colors = cls$colors, labels = cls$labels, opacity = trans, title = title)

    } else if (var == "bsm") {
        lf <- lf %>% addRasterImage(x = rst2, opacity = trans, group = title, colors = cols2) %>%
            leaflet::addLayersControl(overlayGroups = c("Cell locations", title), position = "topleft", options = layersControlOptions(collapsed = FALSE)) %>%
            addLegend(colors = cols, labels = as.character(lvls$cell), opacity = trans, title = title)
    } else if (var == "empty") {
        lf <- lf %>% leaflet::addLayersControl(overlayGroups = c("Cell locations"), position = "topleft")
    } else {


        if (var_as_discrete) {
            lf <- lf %>% addRasterImage(x = rst2, opacity = trans, group = title, colors = cols2) %>%
                leaflet::addLayersControl(overlayGroups = c("Cell locations", title), position = "topleft", options = layersControlOptions(collapsed = FALSE)) %>%
                addLegend(colors = cols2, labels = labels2, opacity = trans, title = title)
        } else {
            lf <- lf %>% addRasterImage(x = rst2, opacity = trans, group = title, colors = pal2) %>%
                leaflet::addLayersControl(overlayGroups = c("Cell locations", title), position = "topleft", options = layersControlOptions(collapsed = FALSE)) %>% addLegend(pal = pal2, values = rng, opacity = trans, title = title)

        }


    }

    lf %>% addPolygons(data = rect, color = "#000000", weight = 1, fill = FALSE)
}
