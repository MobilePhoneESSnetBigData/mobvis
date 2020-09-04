#' Map signal strength
#'
#' Map signal strength
#'
#' @param rst rst
#' @param dt dt
#' @param cp cp
#' @param cells cells
#' @param region region
#' @param type type
#' @param interactive interactive
#' @export
map_sig_strength <- function(rst, dt, cp, cells = NA, region = NULL, type = c("dBm", "s"), interactive = TRUE, settings = mobvis_settings()) {

    cells_highlight <- if (is.na(cells[1])) character() else cells

    if (is.na(cells[1])) cells <- dt$cell

    dtsel <- dt[cell %in% cells, ][, list(dBm = max(dBm), s = max(s)), by = rid]

    p = create_p_raster(rst, dtsel, type = type)


    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }

    map_mob_cells(cp, p, var = type, borders = region, cells = cells_highlight, interactive = interactive, settings = settings)
}



map_pag <- function(rst, dt, cp, cells = NA, region = NULL, interactive = TRUE, settings = mobvis_settings()) {
    type <- "pag"

    cells_highlight <- if (is.na(cells[1])) character() else cells

    if (is.na(cells[1])) cells <- dt$cell

    dtsel <- dt[cell %in% cells, ][, list(pag = max(pag)), by = rid]

    p = create_p_raster(rst, dtsel, type = type)

    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }

    map_mob_cells(cp, p, var = type, borders = region, cells = cells_highlight, interactive = interactive, settings = settings)
}

