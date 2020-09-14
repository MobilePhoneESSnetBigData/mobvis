#' Settings of mobvis
#'
#' Settings of mobvis. The default values of \code{mobvis_settings} are aimed for interactive maps, whereas the values for \code{mobvis_settings_static} are aimed for statis maps.
#'
#' @param titles default titles. Named character vector, where the names should be dBm, s, bsm, pag, pg and pga.
#' @param palettes default palettes. Should be a list with the elements called: dBm, s, bsm, pag, pg and pga.
#' @param palette default palette for non-standard plots
#' @param style the style of the color classes, e.g. "pretty" for pretty rounded breaks, and "cont" for continuous classes. See the \code{style} argument of \code{\link[tmap:tm_raster]{tm_raster}} for all options.
#' @param cell_colors either a single color for all cells, or a vector of colors named: Selected, Small cell and Normal cell
#' @param cell_size size of the cell(s)
#' @param cell_shape shape of the cell(s), by default a dot. Run the last example from \code{\link[tmap:tm_symbols]{tm_symbols}} to see all options.
#' @param cell_offset offset of the cell(s). If not 0, the cells are moved into the propagation direction
#' @param cell_legend should the legend for the different cells be included?
#' @param cell_labels logical that determines whether the cell labels are printed (default \code{FALSE})
#' @param cell_label_color color of the cell labels
#' @param region.lwd line width of the region borders
#' @param dev_size size of the device(s)
#' @param dev_color color of the device(s)
#' @param dev_shape shape of the device(s)
#' @param use_classes logical that determines whether the signal strength (dBm) and dominance (s) should be plot in classes (default) or using a sequential palette. The classes are defined by dBm_classes and s_classes
#' @param dBm_classes list that defines the classes for the signal strength
#' @param s_classes list that defines the classes for the signal dominance
#' @param basemaps basemap(s) for interactive maps (see leaflet::providers and \url{https://leaflet-extras.github.io/leaflet-providers/preview/} for options). It can also be a vector of basemaps. In that case, the user can switch between them. If the CRS is missing in the spatial objects, the basemaps are turned off.
#' @param ... passed on to mobvis_settings
#' @export
#' @name mobvis_settings
#' @rdname mobvis_settings
mobvis_settings <- function(titles = c(dBm = "Signal strength in dBm",
                                    s = "Signal dominance - s",
                                    bsm = "Best server map",
                                    pg = "Prior - P(g)",
                                    pag = "Connection likelihood - P(a|g)",
                                    pga = "Location posterior - P(g|a)"),
                         palettes = list(
                             dBm = "YlGnBu",
                             s = "-YlGnBu",
                             bsm = "Set2",
                             pg = "-YlGnBu",
                             pag = "-YlGn",
                             pga = "-YlOrBr"
                         ),
                         palette = "-YlGnBu",
                         style = "pretty",
                         cell_colors = c("Selected" = "red", "Small cell" = "black", "Normal cell" = "black"),
                         cell_size = .5,
                         cell_shape = 19,
                         cell_offset = 75,
                         cell_legend = FALSE,
                         cell_labels = FALSE,
                         cell_label_color = "black",
                         region.lwd = 3,
                         dev_size = .5,
                         dev_color = "purple",
                         dev_shape = 15,
                         use_classes = TRUE,
                         dBm_classes = list(breaks = c(-Inf, seq(-120, -70, by = 10), Inf),
                                             labels = c("-120 or less", "-120 to -110", "-110 to -100",
                                                        "-100 to -90", "-90 to -80", "-80 to -70", "-70 or better"),
                                             colors = RColorBrewer::brewer.pal(7, "Spectral"),
                                             lims = c(-120, -70),
                                             tit = "Signal strength (dBm)"),
                         s_classes = list(breaks = seq(0, 1, by = .1),
                                             labels = paste(sprintf("%.1f", seq(0, 1, by = .1)[1:10]), "to", sprintf("%.1f", seq(0, 1, by = .1)[2:11])),
                                             colors = RColorBrewer::brewer.pal(10, "Spectral"),
                                             lims = c(0, 1)),
                         basemaps = "OpenStreetMap") {
    as.list(environment())
}

#' @export
#' @name mobvis_settings_interactive
#' @rdname mobvis_settings
mobvis_settings_interactive <- function(cell_colors = c("Selected" = "red", "Small cell" = "goldenrod3", "Normal cell" = "gold"),
                                   cell_offset = 150,
                                   cell_legend = TRUE,
                                   region.lwd = 2,
                                   ...) {
    do.call(mobvis_settings, c(list(cell_colors = cell_colors, cell_offset = cell_offset, cell_legend = cell_legend, region.lwd = region.lwd), list(...)))
}

#' @export
#' @name mobvis_settings_animation
#' @rdname mobvis_settings
mobvis_settings_animation <- function(style = "cont",
                                      ...) {
    do.call(mobvis_settings, c(list(style = style), list(...)))
}
