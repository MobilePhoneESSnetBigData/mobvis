#' Settings of mobvis
#'
#' Settings of mobvis
#'
#' @param titles default titles. Named character vector, where the names should be dBm, s, bsm, pag, pg and pga.
#' @param palettes default palettes. Should be a list with the elements called: dBm, s, bsm, pag, pg and pga.
#' @param palette default palette for non-standard plots
#' @param cell_colors either a single color for all cells, or a vector of colors named: Selected, Small cell and Normal cell
#' @param cell_size size of the cell
#' @param cell_offset offset of the cells. If not 0, the cells are moved into the propagation direction
#' @param cell_legend should the legend for the different cells be included?
#' @param use_classes logical that determines whether the signal strength (dBm) and dominance (s) should be plot in classes (default) or using a sequential palette. The classes are defined by dBm_classes and s_classes
#' @param dBm_classes list that defines the classes for the signal strength
#' @param s_classes list that defines the classes for the signal dominance
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
                             dBm = "Blues",
                             s = "-Blues",
                             bsm = "Set2",
                             pg = "-Blues",
                             pag = "-Greens",
                             pga = "viridis"
                         ),
                         palette = "-Blues",
                         cell_colors = c("Selected" = "red", "Small cell" = "goldenrod3", "Normal cell" = "gold"),
                         cell_size = .5,
                         cell_offset = 150,
                         cell_legend = length(cell_colors) > 1,
                         cell_labels = FALSE,
                         cell_label_color = "black",
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
                                             lims = c(0, 1))) {
    as.list(environment())
}

#' @export
#' @name mobvis_settings_static
#' @rdname mobvis_settings
mobvis_settings_static <- function(cell_colors = c("Selected" = "red", "Small cell" = "black", "Normal cell" = "black"),
                                   cell_size = .5,
                                   cell_offset = 0,
                                   cell_legend = FALSE,
                                   ...) {
    do.call(mobvis_settings, c(list(cell_colors = cell_colors, cell_size = cell_size, cell_offset = cell_offset, cell_legend = cell_legend), list(...)))
}

