
mobloc_crop_raster <- function(r, bbx) {
    raster::crop(r, extent(as.vector(bbx)[c(1,3,2,4)]))
}


mobloc_filter_cell <- function(x, a, raster = NULL) {
    cell <- rid <- NULL

    if (inherits(x, "data.table")) {
        y <- x[cell %in% a]
    } else {
        y <- x[x$cell %in% a, ]
    }

    if (!missing(raster)) {
        if (inherits(x, "data.table")) {
            y[rid %in% raster[]]
        } else {
            y[y$rid %in% raster[], ]
        }
    } else {
        y
    }
}
