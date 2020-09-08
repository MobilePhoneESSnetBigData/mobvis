create_connection_lines <- function(cp1, cp2) {
    c1 <- st_coordinates(cp1)
    c2 <- st_coordinates(cp2)

    st_sf(geometry = do.call(st_sfc, lapply(1:nrow(c1), function(i) {
        co <- rbind(c1[i,],
                    c2[i,])
        st_linestring(co)
    })), cell = cp1$cell, crs = st_crs(cp1))
}


base_tmap <- function(cp, borders = NULL, basemaps = "OpenStreetMap", cells = character(), settings = mobvis_settings()) {
    cp$sel = factor(ifelse(cp$cell %in% cells, "Selected",
                    ifelse(cp$small, "Small cell", "Normal cell")), levels = c("Selected", "Small cell", "Normal cell"))
    cp = cp[, c("sel", "small", "direction", "x", "y", "cell")]

    cp2 <- move_cells_into_prop_direction(cp, settings$cell_offset)
    cp_lines <- create_connection_lines(cp, cp2)

    if (is.na(basemaps[1])) {
        # not working yet
#        tmap_options(projection = 0, basemaps = NULL)
        tm <- list()
    } else {
        tm <- tm_basemap("OpenStreetMap")
    }


    cell_palette = settings$cell_colors
    cell_legend = settings$cell_legend


    if (settings$cell_offset > 0) {
        tm <- tm + tm_shape(cp_lines) +
            tm_lines(col = "#777777", lwd = 3 + sqrt(settings$cell_size), group = "Cell locations", interactive = FALSE, zindex = 401)

        if (settings$cell_labels) {
            tm <- tm + tm_shape(cp2) +
                tm_dots("sel", palette = cell_palette, size = .04 * settings$cell_size, border.col = "black", group = "Cell locations", title = "Cell locations", interactive = TRUE, id = "cell", popup.vars = FALSE, drop.levels = TRUE, zindex = 402, legend.show = cell_legend) +
                tm_text("cell", xmod = 0.5, ymod = 0.5, col = settings$cell_label_color)
        } else {
            tm <- tm + tm_shape(cp2) +
                tm_dots("sel", palette = cell_palette, size = .04 * settings$cell_size, border.col = "black", group = "Cell locations", title = "Cell locations", interactive = TRUE, id = "cell", popup.vars = FALSE, drop.levels = TRUE, zindex = 402, legend.show = cell_legend)
        }

    } else {
        if (settings$cell_labels) {
            tm <- tm + tm_shape(cp) +
                tm_dots("sel", palette = cell_palette, size = .04 * settings$cell_size, border.col = "black", group = "Cell locations", title = "Cell locations", interactive = TRUE, id = "cell", popup.vars = FALSE, drop.levels = TRUE, zindex = 402, legend.show = cell_legend) +
                tm_text("cell", xmod = 0.5, ymod = 0.5, col = settings$cell_label_color)
        } else {
            tm <- tm + tm_shape(cp) +
                tm_dots("sel", palette = cell_palette, size = .04 * settings$cell_size, border.col = "black", group = "Cell locations", title = "Cell locations", interactive = TRUE, id = "cell", popup.vars = FALSE, drop.levels = TRUE, zindex = 402, legend.show = cell_legend)
        }

    }

    if (!is.null(borders)) {
        #tm <- tm + tm_shape(borders) + tm_polygons(col = NA, border.col = "black", group = "Cell locations", interactive = FALSE, alpha = 0)
        tm <- tm + tm_shape(borders, is.master = TRUE) + tm_borders(col = "black", group = "Cell locations", zindex = 403)
    }
    tm
}



#' Low-level level to visualize mobile phone cells and auxiliary data
#'
#' Visualize mobile phone cells and auxiliary data, such as signal strength, location estimation variables. This function is used in \code{\link{explore_mobloc}}, and also by dedicated functions such as \code{\link{map_sig_strength}}.
#'
#' @param cp cellplan
#' @param rst raster data
#' @param var if specified, \code{title} and \code{palette} are specified accordingly. Possible values: \code{"dBm", "s", "pga", "pag", "pg", "bsm"}.
#' @param title title
#' @param palette palette The default depends on \code{var}: \code{-Blues} (from ColorBrewer) for unspecified \code{var}, \code{-Greens} for \code{"pag"}, \code{-Blues} for \code{"pg"}, \code{viridis} for \code{"pga"}, \code{Set2} for \code{"bsm"}.
#' @param cells cells to select
#' @param borders borders (polygon) of the region of interest
#' @param interactive should the map be interactive or static?
#' @param basemaps basemaps used in the interactive map
#' @param opacity the opacity of the raster layer.
#' @param proxy should the map be updated in a Shiny app?
#' @param settings mobvis settings
#' @import tmap
#' @export
map_mob_cells = function(cp, rst, var = NULL, title = NA, palette = NA, cells = character(), borders = NULL, interactive = TRUE, basemaps = "OpenStreetMap", opacity = 1, proxy = FALSE, settings = mobvis_settings()) {
    # check required columns
    if (!all(c("cell", "small", "direction", "x", "y") %in% names(cp))) stop("cp does not contain all the required columns: cell, small, direction, x, and y")

    tmap_mode(ifelse(interactive, "view", "plot"))

    tm2 <- base_tmap(cp = cp,
                     borders = if (proxy) NULL else borders,
                     basemaps = if (proxy) NA else basemaps, cells = cells, settings = settings)


    cell_colors <- settings$cell_colors
    # create palette for cells
    if (!is.null(names(cell_colors))) {
        cell_palette = cell_colors[match(c("Selected", "Small cell", "Normal cell"), names(cell_colors))]
    } else {
        cell_palette = rep(cell_colors, length.out = 3)
    }

    # check var, title and palette
    if (!is.null(var)) {
        if (!(var %in% c("dBm", "s", "pga", "pag", "pg", "bsm", "none"))) stop("unknown var")
    } else {
        var <- "custom"
    }

    if (var != "none") {
        if (var == "bsm") {
            tmap_options(max.categories = 1000)
            appendix <- ""
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
            title <- if (var %in% c("custom", "none")) "" else paste0(unname(settings$titles[var]), appendix)
        }

        if (all(is.na(rst[]))) var <- "empty"

        if (is.na(palette[1])) {
            palette <- if (var %in% c("custom", "none", "empty")) settings$palette else unname(settings$palettes[[var]])
        }

        cls <- if (var == "dBm")  {
            settings$dBm_classes
        } else {
            settings$s_classes
        }


        if (var %in% c("dBm", "s") && settings$use_classes) {
            tm <- tm_shape(rst) +
                tm_raster(names(rst)[1], palette = cls$colors, alpha = opacity, title = title, breaks = cls$breaks, labels = cls$labels, group = "Data", zindex = 404)
        } else if (var != "empty") {
            tm <- tm_shape(rst) +
                tm_raster(names(rst)[1], palette = palette, alpha = opacity, n = 7, stretch.palette = FALSE, title = title, group = "Data", zindex = 404, legend.show = (var != "bsm"))
            if (var == "bsm") {
                tm <- tm + tm_layout(main.title = title)
            }
        }

    } else {
        tm <- list()
        title <- ""
    }

    if (proxy) {
        tmapProxy("map", x = tm_remove_layer(402) + tm_remove_layer(404) + tm2 + tm)
    } else {
        tm + tm2 + tm_layout(legend.outside = TRUE, frame = FALSE)
    }
}
