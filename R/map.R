create_connection_lines <- function(cp1, cp2) {
    c1 <- st_coordinates(cp1)
    c2 <- st_coordinates(cp2)

    st_sf(geometry = do.call(st_sfc, lapply(1:nrow(c1), function(i) {
        co <- rbind(c1[i,],
                    c2[i,])
        st_linestring(co)
    })), cell = cp1$cell, crs = st_crs(cp1))
}


base_tmap <- function(cp, region = NULL, basemaps = "OpenStreetMap", cells = character(), dev  = NULL, settings = mobvis_settings()) {
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
        if (is.na(st_crs(cp))) {
            tm <- tm_basemap(NULL)
        } else {
            tm <- tm_basemap("OpenStreetMap")
        }
    }

    cell_palette = settings$cell_colors
    cell_legend = settings$cell_legend


    if (settings$cell_offset > 0) {
        tm <- tm + tm_shape(cp_lines) +
            tm_lines(col = settings$cell_connection_col, lwd = settings$cell_lwd * 3, group = "Cell locations", interactive = FALSE, zindex = 401)

        if (settings$cell_labels) {
            tm <- tm + tm_shape(cp2) +
                tm_dots(col = "sel", shape = 21, palette = cell_palette, size = .04 * settings$cell_size, border.lwd = settings$cell_lwd, border.col = settings$cell_border_col, group = "Cell locations", title = "Cell locations", interactive = TRUE, id = "cell", popup.vars = FALSE, drop.levels = TRUE, zindex = 402, legend.show = cell_legend) +
                tm_text("cell", xmod = 0.5, ymod = 0.5, col = settings$cell_label_color)
        } else {
            tm <- tm + tm_shape(cp2) +
                tm_dots(col = "sel", shape = 21, palette = cell_palette, size = .04 * settings$cell_size, border.lwd = settings$cell_lwd, border.col = settings$cell_border_col, group = "Cell locations", title = "Cell locations", interactive = TRUE, id = "cell", popup.vars = FALSE, drop.levels = TRUE, zindex = 402, legend.show = cell_legend)
        }

    } else {
        if (settings$cell_labels) {
            tm <- tm + tm_shape(cp) +
                tm_dots(col = "sel", shape = 21, palette = cell_palette, size = .04 * settings$cell_size, shape = settings$cell_shape, border.lwd = settings$cell_lwd, border.col = settings$cell_border_col, group = "Cell locations", title = "Cell locations", interactive = TRUE, id = "cell", popup.vars = FALSE, drop.levels = TRUE, zindex = 402, legend.show = cell_legend) +
                tm_text("cell", xmod = 0.5, ymod = 0.5, col = settings$cell_label_color)
        } else {
            tm <- tm + tm_shape(cp) +
                tm_dots(col = "sel", shape = 21, palette = cell_palette, size = .04 * settings$cell_size, shape = settings$cell_shape, border.lwd = settings$cell_lwd, border.col = settings$cell_border_col, group = "Cell locations", title = "Cell locations", interactive = TRUE, id = "cell", popup.vars = FALSE, drop.levels = TRUE, zindex = 402, legend.show = cell_legend)
        }

    }

    if (!is.null(dev)) {
        tm <- tm + tm_shape(dev) +
            tm_symbols(size = .04 * settings$dev_size, col = settings$dev_color, shape = settings$dev_shape)
    }
    #return(tm)

    if (!is.null(region)) {
        if (!all(sf::st_is_valid(region))) {
            region = sf::st_make_valid(region)
        }
        if (all(sf::st_is_valid(region))) {
            tm <- tm + tm_shape(region, is.master = TRUE) + tm_borders(col = "black", group = "Cell locations", zindex = 403, lwd = settings$region.lwd)
        }
        #tm <- tm + tm_shape(region) + tm_polygons(col = NA, border.col = "black", group = "Cell locations", interactive = FALSE, alpha = 0)
    }
    tm
}



#' Low-level level to visualize mobile phone cells and auxiliary data
#'
#' Visualize mobile phone cells and auxiliary data, such as signal strength, location estimation variables. This function is used in \code{\link{explore_mobloc}}, and also by dedicated functions such as \code{\link{map_sig_strength}}.
#'
#' @param cp cellplan
#' @param rst raster data
#' @param var if specified, \code{title} and \code{palette} are specified accordingly. Possible values: \code{"dBm", "s", "pga", "pag", "pg", "p", "bsm"}.
#' @param title title
#' @param palette palette The default depend is taken from the \code{settings}
#' @param cells cells to select
#' @param region borders (polygon) of the region of interest
#' @param dev \code{sf} object of device(s)
#' @param interactive should the map be interactive or static?
#' @param opacity the opacity of the raster layer.
#' @param proxy should the map be updated in a Shiny app?
#' @param settings mobvis settings
#' @param ... arguments passed on to \code{\link[tmap:tm_raster]{tm_raster}}
#' @import tmap
#' @export
map_mob_cells = function(cp, rst, var = NULL, title = NA, palette = NA, cells = character(), region = NULL, dev = NULL, interactive = TRUE, opacity = 1, proxy = FALSE, settings = mobvis_settings(), ...) {
    # check required columns
    if (!all(c("cell", "small", "direction", "x", "y") %in% names(cp))) stop("cp does not contain all the required columns: cell, small, direction, x, and y")
    suppressMessages(tmap_mode(ifelse(interactive, "view", "plot")))

    tm2 <- base_tmap(cp = cp,
                     region = if (proxy) NULL else region,
                     dev = dev,
                     basemaps = if (proxy) NA else settings$basemaps, cells = cells, settings = settings)


    cell_colors <- settings$cell_colors
    # create palette for cells
    if (!is.null(names(cell_colors))) {
        cell_palette = cell_colors[match(c("Selected", "Small cell", "Normal cell"), names(cell_colors))]
    } else {
        cell_palette = rep(cell_colors, length.out = 3)
    }

    # check var, title and palette
    if (!is.null(var)) {
        if (!(var %in% c("dBm", "s", "pga", "pag", "pg", "p", "bsm", "none"))) stop("unknown var")
    } else {
        var <- "custom"
    }

    line_br <- ifelse(interactive, "<br>", "\n")


    if (var != "none") {
        if (var == "bsm") {
            tmap_options(max.categories = 1000)
            appendix <- ""
        } else if (!is.factor(rst)) {
            values <- rst[]

            if (var %in% c("pga", "pg", "pag", "p")) {
                if (is.na(settings$prob_th)) {
                    values[is.na(values)] <- 0
                } else {
                    values[which(values < settings$prob_th)] <- NA
                }
                oneValue = (length(na.omit(unique(values))) <= 1)
            } else if (var == "dBm" && is.na(settings$prob_th)) {
                    values[is.na(values)] <- -130
            } else if (var == "s" && is.na(settings$prob_th)) {
                values[is.na(values)] <- 0
            }


            if (all(is.na(values))) {
                rst[] <- NA
                appendix <- ""

            } else {
                maxv <- max(values, na.rm = TRUE)
                minv <- min(values, na.rm = TRUE)
                allOnes <- (minv > .999 && maxv <= 1.001)
                inThousands <- (maxv > 0.01 && maxv < 0.1 && minv >= 0)
                inMillions <- (maxv <= 0.01 && minv >= 0)
                if (allOnes && is.na(title)) {
                    values2 <- values
                    values2[values2 > .999 & values2 <= 1.001] <- 1
                    #values <- pmin(pmax(rst[], 0), 1)
                } else if (inThousands && is.na(title)) {
                    values2 <- values * 1000
                } else if (inMillions && is.na(title)) {
                    values2 <- values * 1000000
                } else {
                    values2 <- values
                }
                rst[] <- values2
                appendix <- ifelse(allOnes || (!inThousands && !inMillions), "", paste0(line_br, "(in 1 / ", ifelse(inThousands, "1,000", "1,000,000"), ")"))
            }
        }

        if (is.na(title)) {
            title <- if (var %in% c("custom", "none")) "" else paste0(unname(settings$titles[var]), appendix)
        }

        if (all(is.na(rst[]))) var <- "empty"

        if (is.na(palette[1])) {
            palette <- if (var %in% c("custom", "none", "empty", "p")) settings$palette else unname(settings$palettes[[var]])
        }

        brewerNames <- c(rownames(RColorBrewer::brewer.pal.info), paste0("-", rownames(RColorBrewer::brewer.pal.info)))
        viridisNames <- c("viridis", "magma", "plasma", "inferno", "cividis")
        if (var == "bsm") {
            rstmx = max(rst[], na.rm = TRUE)
            if (substr(palette[1], 1, 11) == "ColorBrewer") {
                #palette = colorRampPalette(c(RColorBrewer::brewer.pal(12, "Paired"), RColorBrewer::brewer.pal(8, "Set2")))(rstmx)
                randomize <- (nchar(palette[1]) > 11)

                if (randomize) set.seed(as.numeric(substr(palette[1], 12, nchar(palette[1]))))

                palette = c(RColorBrewer::brewer.pal(12, "Paired"), RColorBrewer::brewer.pal(8, "Set2"), RColorBrewer::brewer.pal(9, "Set1"), RColorBrewer::brewer.pal(12, "Set3"))
                palette <- rep(palette, length.out = rstmx)
                if (randomize) palette = sample(palette)
            } else if (palette[1] == "hcl") {
                palette = hcl(h = spread(rstmx) / rstmx * 360, c = 90, l = 70)
            } else if (palette[1] == "rainbow") {
                palette = rainbow(rstmx)[spread(rstmx)]
            } else if (palette[1] %in% viridisNames) {
                palette <- do.call(palette[1], list(n = rstmx))
            } else if (palette[1] %in% brewerNames) {
                palette <- tmaptools::get_brewer_pal(palette[1], n = rstmx, plot = FALSE)
            } else {
                palette <- rep(palette, length.out = rstmx)
            }
            palette = palette[spread(rstmx)]

        }

        if (var %in% c("pga", "pg", "pag", "p")) {
            # extract middle palette value
            if (oneValue) {
                if (palette[1] %in% viridisNames) {
                    palette <- do.call(palette[1], list(n = 7))
                } else if (palette[1] %in% brewerNames) {
                    palette <- tmaptools::get_brewer_pal(palette[1], n = 7, plot = FALSE)
                }
                palette = palette[(length(palette)+1)/2]
            }
        }

        cls <- if (var == "dBm")  {
            settings$dBm_classes
        } else {
            settings$s_classes
        }

        if (var %in% c("dBm", "s") && settings$use_classes) {
            tm <- tm_shape(rst) +
                tm_raster(names(rst)[1], palette = cls$colors, alpha = opacity, title = title, breaks = cls$breaks, labels = cls$labels, group = "Data", zindex = 404, ...)
        } else if (var != "empty") {
            tm <- tm_shape(rst) +
                tm_raster(names(rst)[1], palette = palette, style = settings$style, alpha = opacity, n = 7, stretch.palette = FALSE, title = title, group = "Data", zindex = 404, legend.show = (var != "bsm"), ...)
            if (var == "bsm") {
                tm <- tm + tm_layout(main.title = title)
            }
        } else {
            tm <- list()
        }

    } else {
        tm <- list()
        title <- ""
    }
    if (proxy) {
        tmapProxy("map", x = tm_remove_layer(402) + tm_remove_layer(404) + tm2 + tm)
    } else {
        tm + tm2 + tm_layout(legend.outside = TRUE, frame = FALSE, bg.color = settings$bg.color)
    }
}

# from treemap
spread <- function (n)
{
    if (n < 5) {
        s <- 1:n
        if (n > 2)
            s[2:3] <- 3:2
    }
    else {
        s.step <- floor(n/(2.5))
        s <- seq(1, by = s.step, length.out = n)
        s <- s%%n
        s[s == 0] <- n
        dup <- which(duplicated(s))[1]
        if (!is.na(dup))
            s <- s + rep(0:(s.step - 1), each = (dup - 1), length.out = n)
    }
    s
}
