#' Animate the trajectory of devices
#'
#' Animate the trajectory of devices, while showing the likelihood/event location probabilities (pag), the posterior probabilties (pg) or any other probabilities p. See the vignette of the simulator how they are used.
#'
#' @param rst raster
#' @param dt data.table for \code{animate_pag} and \code{animate_pga} the following columns should be present: cell, rid, dist and either pag or pga. For \code{animate_p} the columns should be \code{t}, \code{dev}, \code{rid} and \code{p}.
#' @param traj sf object with the following columns: \code{t}, \code{cell}, \code{dev}, and \code{geometry} (the sf geometry column with points).
#' @param cp sf object of the cellplan
#' @param region sf object containing the region borders
#' @param settings settings of the animation. By default \code{\link{mobvis_settings_animation}}
#' @param title title of the animation. Can be either gif or mp4. Use the {percentage}s for the device id.
#' @param filename filename. Can be either gif or mp4. Use {percentage sign}s for the time id.
#' @param width width of the animation
#' @param height height of the animation
#' @param fps frame per second
#' @export
#' @rdname animate
#' @name animate_pag
animate_pag <- function(rst,
                        dt,
                        traj,
                        cp,
                        region = NULL,
                        settings = mobvis_settings_animation(),
                        title = NA,
                        filename = "event_location_dev_%s.mp4",
                        width = 700,
                        height = 700,
                        fps = 3) {
    animate_mob(rst = rst,
                dt = dt,
                traj = traj,
                cp = cp,
                region = region,
                settings = settings,
                title = title,
                filename = filename,
                width = width,
                height = width,
                fps =fps,
                var = "pag")
}

#' @export
#' @rdname animate
#' @name animate_pga
animate_pga <- function(rst,
                        dt,
                        traj,
                        cp,
                        region = NULL,
                        settings = mobvis_settings_animation(),
                        title = NA,
                        filename = "posterior_dev_%s.mp4",
                        width = 700,
                        height = 700,
                        fps = 3) {
    animate_mob(rst = rst,
                dt = dt,
                traj = traj,
                cp = cp,
                region = region,
                settings = settings,
                title = title,
                filename = filename,
                width = width,
                height = width,
                fps =fps,
                var = "pga")
}

#' @export
#' @rdname animate
#' @name animate_p
animate_p <- function(rst,
                      dt,
                      traj,
                      cp,
                      region = NULL,
                      settings = mobvis_settings_animation(),
                      title = NA,
                      filename = "prob_dev_%s.mp4",
                      width = 700,
                      height = 700,
                      fps = 3) {
    animate_mob(rst = rst,
                dt = dt,
                traj = traj,
                cp = cp,
                region = region,
                settings = settings,
                title = title,
                filename = filename,
                width = width,
                height = width,
                fps =fps,
                var = "p")
}


animate_mob <- function(rst,
                        dt,
                        traj,
                        cp,
                        region = NULL,
                        settings = mobvis_settings_animation(),
                        title = NA,
                        filename = "animation.mp4",
                        width = 700,
                        height = 700,
                        fps = 3,
                        var = c("pag", "pga", "p")) {

    cell <- NULL
    if (!var %in% c("pag", "pga", "p")) stop("var should be pag, pga, or p")
    cells <- traj$cell

    if (var =="p") {
        mv <- max(dt$p)
    } else {
        mv <- max(dt[cell %in% cells, ][[var]])
    }

    brks <- pretty(c(0, mv), n = 8)
    fun <- if (var == "pag") {
        map_pag
    } else if (var == "pga") {
        map_pga
    } else {
        map_p
    }

    ts <- sort(unique(traj$t))

    maps <- lapply(ts, function(i) {
        ti = traj %>% filter(t == i)
        if (var == "p") {
            dti = dt[t == i, ]
        } else {
            dti = dt[cell%in%ti$cell, ]
        }
        fun(rst,
            dt = dti,
            cp = cp,
            region = region,
            cells = ti$cell,
            dev = ti,
            interactive = FALSE,
            title = sprintf(title, ti$t[1]),
            settings = settings,
            breaks = brks)
    })
    filename <- sprintf(filename, traj$dev[1])
    tmap_animation(maps, filename = filename, width = width, height = height, fps = fps)
}
