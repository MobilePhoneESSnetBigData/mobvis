#' Map signal strength (dBm), signal dominance (s), prior (pg), likelihood (pag), and posterior (pga).
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
#' @param settings settings
#' @rdname map_functions
#' @name map_sig_strength
#' @export
map_sig_strength <- function(rst, dt, cp, cells = NA, region = NULL, dev = NULL, type = c("dBm", "s"), interactive = TRUE, settings = mobvis_settings()) {

    cells_highlight <- if (is.na(cells[1])) character() else cells

    if (is.na(cells[1])) cells <- dt$cell

    dtsel <- dt[cell %in% cells, ][, list(dBm = max(dBm), s = max(s)), by = rid]

    p = create_p_raster(rst, dtsel, type = type)


    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }

    map_mob_cells(cp, p, var = type, region = region, cells = cells_highlight, dev = dev, interactive = interactive, settings = settings)
}

#' @rdname map_functions
#' @name map_bast_server
#' @export
map_best_server <- function(rst, dt, cp, cells = NA, region = NULL, dev = NULL, type = c("dBm", "s"), interactive = TRUE, settings = mobvis_settings()) {

    cells_highlight <- if (is.na(cells[1])) character() else cells

    if (is.na(cells[1])) cells <- dt$cell

    dt[, cell:=as.factor(cell)]
    dt[, pag:=dBm]
    bsm <- mobloc::create_best_server_map(dt, rst)


    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }

    map_mob_cells(cp, bsm, var = type, region = region, cells = cells_highlight, dev = dev, interactive = interactive, settings = settings)
}




#' @rdname map_functions
#' @name map_pg
#' @export
map_pg <- function(rst, cp, region = NULL, dev = NULL, interactive = TRUE, settings = mobvis_settings()) {
    type <- "pag"
    p = rst
    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }
    map_mob_cells(cp, p, var = type, region = region, dev = dev, interactive = interactive, settings = settings)
}


#' @rdname map_functions
#' @name map_pag
#' @export
map_pag <- function(rst, dt, cp, cells = NA, region = NULL, dev = NULL, interactive = TRUE, settings = mobvis_settings()) {
    type <- "pag"

    cells_highlight <- if (is.na(cells[1])) character() else cells

    if (is.na(cells[1])) cells <- dt$cell

    dtsel <- dt[cell %in% cells, ][, list(pag = max(pag)), by = rid]

    p = create_p_raster(rst, dtsel, type = type)

    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }

    map_mob_cells(cp, p, var = type, region = region, cells = cells_highlight, dev = dev, interactive = interactive, settings = settings)
}


#' @rdname map_functions
#' @name map_pga
#' @export
map_pga <- function(rst, dt, cp, cells = NA, region = NULL, dev = NULL, interactive = TRUE, settings = mobvis_settings()) {
    type <- "pga"

    cells_highlight <- if (is.na(cells[1])) character() else cells

    if (is.na(cells[1])) cells <- dt$cell

    dtsel <- dt[cell %in% cells, ][, list(pga = max(pga)), by = rid]

    p = create_p_raster(rst, dtsel, type = type)

    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }

    map_mob_cells(cp, p, var = type, region = region, cells = cells_highlight, dev = dev, interactive = interactive, settings = settings)
}

