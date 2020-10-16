#' Map signal strength (dBm), signal dominance (s), prior (pg), likelihood (pag), posterior (pga), and any other probability (p).
#'
#' Map signal strength (dBm), signal dominance (s), prior (pg), likelihood (pag), posterior (pga) and any other probability (p). See the vignettes to see how there are used with the mobloc package and the simulator files.
#'
#' @param rst raster object
#' @param dt dt data.table that contains the data to be mapped
#' @param cp cellplan \code{sf} object of the cells
#' @param cells cells cells for which the data is shown
#' @param region region \code{sf} object that contains the borders of the region. If omitted, the raster borders are shown.
#' @param dev dev \code{sf} object of the location of one or more devices (needed for the animation functions)
#' @param type type \code{"dBm"} for signal strength or \code{"s"} for signal dominance
#' @param interactive should the map be interactive?
#' @param title title of the map. The default value is stored in the settings.
#' @param settings settings of the map. See \code{\link{mobvis_settings}}.
#' @param ... arguments passed on to \code{\link[tmap:tm_raster]{tm_raster}}
#' @rdname map_functions
#' @name map_sig_strength
#' @export
map_sig_strength <- function(rst, dt, cp, cells = NA, region = NULL, dev = NULL, type = c("dBm", "s"), interactive = TRUE, title = NA, settings = mobvis_settings(), ...) {
    cell <- dBm <- s <- rid <- NULL


    cells_highlight <- if (is.na(cells[1])) character() else cells

    if (is.na(cells[1])) cells <- dt$cell

    dtsel <- dt[cell %in% cells, ][, list(dBm = max(dBm), s = max(s)), by = rid]

    p = create_p_raster(rst, dtsel, type = type, settings = settings)


    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }

    map_mob_cells(cp, p, var = type, region = region, cells = cells_highlight, dev = dev, interactive = interactive, title = title, settings = settings, ...)
}

#' @rdname map_functions
#' @name map_bast_server
#' @export
map_best_server <- function(rst, dt, cp, cells = NA, region = NULL, dev = NULL, interactive = TRUE, title = NA, settings = mobvis_settings(), ...) {
    type = "bsm"
    cell <- pag <- dBm <- NULL

    cells_highlight <- if (is.na(cells[1])) character() else cells

    if (is.na(cells[1])) cells <- dt$cell

    dt[, cell:=as.factor(cell)]
    if ("dBm" %in% names(dt)) dt[, pag:=dBm]
    bsm <- mobloc::create_best_server_map(dt, rst)


    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }

    map_mob_cells(cp, bsm, var = type, region = region, cells = cells_highlight, dev = dev, interactive = interactive, title = title, settings = settings, ...)
}




#' @rdname map_functions
#' @name map_pg
#' @export
map_pg <- function(rst, cp, region = NULL, dev = NULL, interactive = TRUE, title = NA, settings = mobvis_settings(), ...) {
    type <- "pg"
    p = rst
    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }
    map_mob_cells(cp, p, var = type, region = region, dev = dev, interactive = interactive, title = title, settings = settings, ...)
}

#' @rdname map_functions
#' @name map_pag
#' @export
map_p <- function(rst, dt, cp, cells = NA, region = NULL, dev = NULL, interactive = TRUE, title = NA, settings = mobvis_settings(), ...) {
    cells_highlight <- if (is.na(cells[1])) character() else cells

    p = create_p_raster(rst, dt, type = "p", settings = settings)

    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }

    map_mob_cells(cp, p, var = "p", region = region, cells = cells_highlight, dev = dev, interactive = interactive, title = title, settings = settings, ...)
}


#' @rdname map_functions
#' @name map_pag
#' @export
map_pag <- function(rst, dt, cp, cells = NA, region = NULL, dev = NULL, interactive = TRUE, title = NA, settings = mobvis_settings(), ...) {

    cell <- pag <- rid <- NULL

    type <- "pag"

    cells_highlight <- if (is.na(cells[1])) character() else cells

    if (is.na(cells[1])) cells <- dt$cell

    dtsel <- dt[cell %in% cells, ][, list(pag = max(pag)), by = rid]

    p = create_p_raster(rst, dtsel, type = type, settings = settings)

    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }

    map_mob_cells(cp, p, var = type, region = region, cells = cells_highlight, dev = dev, interactive = interactive, title = title, settings = settings, ...)
}


#' @rdname map_functions
#' @name map_pga
#' @export
map_pga <- function(rst, dt, cp, cells = NA, region = NULL, dev = NULL, interactive = TRUE, title = NA, settings = mobvis_settings(), ...) {
    cell <- pga <- rid <- NULL

    type <- "pga"

    cells_highlight <- if (is.na(cells[1])) character() else cells

    if (is.na(cells[1])) {
        cells <- dt$cell
    } else if (!any(cells %in% dt$cell)) {
        warning("cell(s) ", cells, " not found in dt$cell")
        cells <- dt$cell
    }

    dtsel <- dt[cell %in% cells, ][, list(pga = max(pga)), by = rid]

    p = create_p_raster(rst, dtsel, type = type, settings = settings)

    if (is.null(region)) {
        region <- create_bbx_rect(raster2bbx(rst))
    }

    map_mob_cells(cp, p, var = type, region = region, cells = cells_highlight, dev = dev, interactive = interactive, title = title, settings = settings, ...)
}

