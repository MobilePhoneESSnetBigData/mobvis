#' Get simulator data
#'
#' Get simulator data
#'
#' @param sim list containing the following elements: \code{input_dir}, \code{output_dir}, \code{crs}.
#' @param rst rst
#' @param cp cp
#' @export
#' @name sim_get_region
#' @rdname sim_data
sim_get_region <- function(sim) {
    x <- st_as_sfc(readLines(file.path(sim$input_dir, "map.wkt")))
    if (is.na(st_crs(x))) {
        st_crs(x) <- sim$crs
    }
    x
}

#' @name sim_get_raster
#' @rdname sim_data
#' @export
sim_get_raster <- function(sim) {
    grd = read.csv(file.path(sim$output_dir, "grid.csv"))
    rst = raster(nrows = grd$No.Tiles.Y,
                 ncols = grd$No.Tiles.X,
                 xmn = grd$Origin.X,
                 ymn = grd$Origin.Y,
                 xmx = grd$Origin.X + (grd$No.Tiles.X * grd$X.Tile.Dim),
                 ymx = grd$Origin.Y + (grd$No.Tiles.Y * grd$Y.Tile.Dim))

    rst[] = 0:(length(rst)-1)
    rst = raster::flip(rst, "y")
    if (is.na(raster::crs(rst))) {
        raster::crs(rst) <- sim$crs$input
    }
    rst
}

#' @name sim_get_cellplan
#' @rdname sim_data
#' @export
sim_get_cellplan <- function(sim) {
    cellplanxml <- xml2::as_list(xml2::read_xml(file.path(sim$input_dir, "antennas.xml")))
    cp = tibble::as_tibble(cellplanxml) %>%
        tidyr::unnest_wider(antennas) %>%
        tidyr::unnest(cols = names(.)) %>%
        tidyr::unnest(cols = names(.)) %>%
        readr::type_convert() %>%
        dplyr::mutate(x2 = x,
                      y2 = y,
                      cell = 1:dplyr::n(),
                      small = FALSE) %>%
        st_as_sf(coords = c("x2", "y2"), crs = sim$crs)

    #ant = read.csv(file.path(sim$output_dir, "antennas.csv"))
    cp
}


#' @name sim_get_signal_strength
#' @rdname sim_data
#' @export
sim_get_signal_strength <- function(sim, rst, cp) {
    x <- readr::read_csv(file.path(sim$output_dir, "SignalMeasure_MNO1.csv")) %>%
        dplyr::rename(cell = "Antenna ID") %>%
        tidyr::pivot_longer(-cell, names_to = "rid", values_to = "dBm") %>%
        dplyr::mutate(rid = as.integer(substr(rid, 5, nchar(rid)))) %>%
        dplyr::left_join(cp %>% st_drop_geometry() %>% dplyr::select(cell, Smid, SSteep), by = "cell") %>%
        dplyr::mutate(s = mobloc::db2s(dBm = dBm, midpoint = Smid, steepness = SSteep)) %>%
        dplyr::select(cell, rid, dBm, s) %>%
        as.data.table()
}


# sim_get_antenna_info <- function(sim, rst, cp) {
#     cp
# }

