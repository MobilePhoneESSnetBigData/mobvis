#' Get simulator data
#'
#' Get simulator data
#'
#' @param sim list containing the following elements: \code{input_dir}, \code{output_dir}, \code{crs}.
#' @param rst rst
#' @param cp cp
#' @param device device id
#' @param t time id
#' @param th probability threshold. Only probabilities of at least `th` are returned
#' @export
#' @name sim_get_region
#' @rdname sim_data
sim_get_region <- function(sim) {
    x <- st_as_sfc(readLines(file.path(sim$input_dir, "map.wkt")))
    if (is.na(st_crs(x)) && !is.na(st_crs(sim$crs))) {
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
    if (is.na(raster::crs(rst)) && !is.na(st_crs(sim$crs))) {
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
    x <- readr::read_csv(file.path(sim$output_dir, paste0("SignalMeasure_", sim$mno, ".csv"))) %>%
        dplyr::rename(cell = "Antenna ID") %>%
        tidyr::pivot_longer(-cell, names_to = "rid", values_to = "dBm") %>%
        dplyr::mutate(rid = as.integer(substr(rid, 5, nchar(rid)))) %>%
        dplyr::left_join(cp %>% st_drop_geometry() %>% dplyr::select(cell, Smid, SSteep), by = "cell") %>%
        dplyr::mutate(s = mobloc::db2s(dBm = dBm, midpoint = Smid, steepness = SSteep)) %>%
        dplyr::select(cell, rid, dBm, s) %>%
        as.data.table()
}

#' @name sim_get_trajectory_data
#' @rdname sim_data
#' @export
sim_get_trajectory_data <- function(sim, device = NULL) {
    f <- file.path(sim$output_dir, paste0("AntennaInfo_MNO_", sim$mno, ".csv"))
    x <- readr::read_csv(f)

    if (is.null(device)) {
        device <- unique(x$`Device ID`)
    } else {
        if (!(all(device %in% x$`Device ID`))) stop("Device ID(s) not found in ", f)
    }

    x %>%
        dplyr::rename(cell = 2, event = 3, dev = 4, tile = 7) %>%
        dplyr::select(t, cell, dev, x, y) %>%
        dplyr::filter(dev == device) %>%
        st_as_sf(coords = c("x", "y"), crs = sim$crs)
}

#' @name sim_get_trajectory_routes
#' @rdname sim_data
#' @export
sim_get_trajectory_routes <- function(sim, device = NULL) {
    f <- file.path(sim$output_dir, paste0("AntennaInfo_MNO_", sim$mno, ".csv"))
    x <- readr::read_csv(f) %>%
        dplyr::rename(cell = 2, event = 3, dev = 4, tile = 7) %>%
        dplyr::select(t, cell, dev, x, y)

    if (is.null(device)) {
        device <- unique(x$dev)
    } else {
        if (!(all(device %in% x$dev))) stop("Device ID(s) not found in ", f)
    }

    dists <- lapply(device, function(d) {
        xd <- x %>%
            filter(dev == d)
        st_linestring(as.matrix(xd[,c("x", "y")]))
    })

    sfc <- st_sfc(dists, crs = sim$crs)
    y <- st_sf(dev = device, geometry = sfc)

    y$distance <- sf::st_length(y)
    y
}

#' @name sim_devices_at_t
#' @rdname sim_data
#' @export
sim_devices_at_t <- function(sim, t) {
    f <- file.path(sim$output_dir, paste0("AntennaInfo_MNO_", sim$mno, ".csv"))
    x <- readr::read_csv(f)

    if (!(t %in% x$t)) stop("No records found for t = ", t, " in ", f)

    x %>%
        dplyr::rename(time = 1, cell = 2, event = 3, dev = 4, tile = 7) %>%
        dplyr::select(time, cell, dev, x, y) %>%
        dplyr::filter(time == t) %>%
        dplyr::rename(t = time) %>%
        st_as_sf(coords = c("x", "y"), crs = sim$crs)
}

# RUN THIS CODE TO SAVE A SUBSET OF PROBABILITIES FOR THE EXAMPLE DATA IN INST
#
# p <- fread("probabilities_network_MNO1.csv")
# setnames(p, "Phone ID", "dev")
# p2 <- p[dev %in% c(210, 250, 741), ]
# setnames(p2, "dev", "Phone ID")
# fwrite(p2, file = "probabilities_network_MNO1_sel.csv")

#' @name sim_get_prob
#' @rdname sim_data
#' @export
sim_get_prob <- function(sim, device, th = 1e-6) {
    p <- fread(file.path(sim$output_dir, paste0("probabilities_network_", sim$mno,".csv")))
    setnames(p, "Phone ID", "dev")
    psel <- p[dev %in% device, ] %>%
        tidyr::as_tibble() %>%
        tidyr::pivot_longer(
            cols = starts_with("Tile"),
            names_to = "rid",
            values_to = "p"
        ) %>%
        dplyr::mutate(rid = as.integer(substr(rid, 5, nchar(rid)))) %>%
        dplyr::filter(p >= th) %>%
        as.data.table()
    psel
}
