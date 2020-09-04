get_grid_coor <- function(range) {

    as.data.frame(expand.grid(x=seq(-range/4, range, by = range / 100), y=seq(-range/2, range/2, by = range/100), z=0))
}

dBm_classes <- list(breaks = c(-Inf, seq(-120, -70, by = 10), Inf),
                    labels = c("-120 or less", "-120 to -110", "-110 to -100",
                               "-100 to -90", "-90 to -80", "-80 to -70", "-70 or better"),
                    colors = RColorBrewer::brewer.pal(7, "Spectral"),
                    lims = c(-120, -70),
                    tit = "Signal strength (dBm)")

qty_classes <- list(breaks = seq(0, 1, by = .1),
                    labels = paste(sprintf("%.1f", seq(0, 1, by = .1)[1:10]), "to", sprintf("%.1f", seq(0, 1, by = .1)[2:11])),
                    colors = RColorBrewer::brewer.pal(10, "Spectral"),
                    lims = c(0, 1),
                    tit = "Signal dominance            ")






heatmap_ground <- function(param_model, param_plots, param, title = TRUE) {
    valueCat <- value <- NULL

    co <- get_grid_coor(range  = param_plots$range)

    lh <- x <- y <- db <- NULL
    if (length(param_plots$enable)) {
        param$midpoint <- param_model$midpoint
        param$steepness <- param_model$steepness

        co2 <- cbind(co, signal_strength(0,0, param_model$height, direction = param_model$direction, tilt = param_model$tilt, beam_h = param_model$h3dB, beam_v =  param_model$v3dB, W = param_model$W, co = co, ple = param_model$ple, param = param, enable = param_plots$enable))


        if (param_plots$type == "dominance") {
            co2$value <- co2$s
            cls <- qty_classes
            mr <- param_plots$maskrangelh
        } else {
            co2$value <- co2$dBm
            cls <- dBm_classes
            mr <- param_plots$maskrangedb
        }


        if (param_plots$mask) co3 <- co2[co2$value >= mr[1] & co2$value <= mr[2], ]


        co2$valueCat <- cut(co2$value, breaks = cls$breaks, labels = cls$labels, include.lowest = TRUE, right = FALSE)

        co2$value[co2$value > cls$lims[2]] <- cls$lims[2]
        co2$value[co2$value < cls$lims[1]] <- cls$lims[1]

        if (param_plots$colors == "discrete") {
            gg <- ggplot(co2, mapping = aes(x=x, y=y, fill = valueCat)) + geom_tile() + coord_fixed() + scale_fill_manual(cls$tit, values = cls$colors, drop = FALSE) # viridisLite::viridis(9, option = "C")
        } else {
            gg <- ggplot(co2, mapping = aes(x=x, y=y, fill = value)) + geom_tile() + coord_fixed() + scale_fill_gradient(cls$tit, limits = cls$lims)
        }

        if (param_plots$mask) {
            gg <- gg + geom_tile(data = co3, fill = "red")
        }


        #gg + geom_point(aes(x=x,y=y, color = col, fill=NA), data = data.frame(x=0, y=0, col="black")) + scale_color_manual("trfsdgsdgsfgdsfa", values = 1)
        if (title) {
            if (is.na(param_model$direction)) {
                gg + ggtitle("Top view of a small cell")
            } else {
                gg + ggtitle("Top view of a normal cell directed eastwards")
            }
        } else {
            gg
        }
    }

}

#' Signal strength model plots
#'
#' The \code{radiation_plot} plots the radiation in the horizontal (azimuth) or vertical (elevation) plane, the \code{distance_plot} the relation between distance and signal loss, and the \code{signal_dominance} plots the relation between signal strength and signal dominance, which is modelled as a logistic function. These plots are embedded in the interactive tool \code{\link{setup_sig_strength_model}}.
#'
#' @name distance_plot
#' @rdname plot_functions
#' @param W power of an cell
#' @param ple path loss exponent
#' @param range range
#' @param base_size base size of the plot
#' @param show_classes show the class colors
#' @seealso \code{\link{setup_sig_strength_model}}
#' @importFrom RColorBrewer brewer.pal
#' @example ./examples/plot_functions.R
#' @export
distance_plot <- function(W, ple, range, base_size = 11, show_classes = TRUE) {
  distance <- fill <- xmin <- xmax <- ymin <- ymax <- NULL
  dBm <- W2dBm(W)
  df <- data.frame(distance = seq(10, range, by=10))
  df$dBm <- distance2dB(df$distance, ple, W)
  df <- df[df$dBm >= -110 & df$dBm <= 0, ]

  if (show_classes) {
      brks <- c(-130, dBm_classes$breaks[2:(length(dBm_classes$breaks) - 1)], 0)
      rangemin <- -range/30
      df2 <- data.frame(xmin = rangemin,
                        xmax = rangemin / 4,
                        ymin = brks[-length(brks)],
                        ymax = brks[-1],
                        fill = dBm_classes$colors)


      ggplot(df, aes(x=distance, y= dBm)) + geom_line() +
          geom_rect(data = df2, aes(fill = I(fill), xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, x = NULL)) +
          scale_x_continuous("Distance (m)", limits = c(rangemin, range), breaks = pretty(c(0, range), n = 8), minor_breaks = NULL) +
          scale_y_continuous("Signal strength (dBm)", limits = c(-130, 0), breaks = seq(-130, 0, by = 10), minor_breaks = NULL) + theme_bw(base_size = base_size) + ggtitle("Signal loss over distance") +
          theme(panel.grid.major = element_line("grey85"))
  } else {
      ggplot(df, aes(x=distance, y= dBm)) + geom_line() +
          scale_x_continuous("Distance (m)", limits = c(0, range), breaks = pretty(c(0, range), n = 8), minor_breaks = NULL) +
          scale_y_continuous("Signal strength (dBm)", limits = c(-130, 0), breaks = seq(-130, 0, by = 10), minor_breaks = NULL) + theme_bw(base_size = base_size) + ggtitle("Signal loss over distance") +
          theme(panel.grid.major = element_line("grey85"))
  }

}

#' @name signal_dominance_plot
#' @rdname plot_functions
#' @param midpoint middle point in the logistic function to map signal strength to probability
#' @param steepness width of the logistic function to map signal strength to probability
#' @export
signal_dominance_plot <- function(midpoint, steepness, base_size = 11, show_classes = TRUE) {
    dBm <- likelihood <- rsig <- fill <- xmin <- xmax <- ymin <- ymax <- NULL


  df <- data.frame(dBm = seq(-130, -50, length.out = 100))
  df$rsig <- db2s(df$dBm, midpoint, steepness)

  if (show_classes) {
      brks <- qty_classes$breaks
      df2 <- data.frame(xmin = -133,
                        xmax = -131,
                        ymin = brks[-length(brks)],
                        ymax = brks[-1],
                        fill = qty_classes$colors)


      brks <- c(-130, dBm_classes$breaks[2:(length(dBm_classes$breaks) - 1)], -50)
      df3 <- data.frame(xmin = brks[-length(brks)],
                        xmax = brks[-1],
                        ymin = -.1,
                        ymax = -.03,
                        fill = dBm_classes$colors)

      ggplot(df, aes(x=dBm, y=rsig)) + geom_line() +
          geom_rect(data = df3, aes(fill = I(fill), xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, x = NULL, y = NULL)) +
          geom_rect(data = df2, aes(fill = I(fill), xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, x = NULL, y = NULL)) +
          scale_y_continuous("Signal dominance", limits = c(-.1, 1), breaks = qty_classes$breaks, minor_breaks = NULL) +
          scale_x_continuous("Signal strength (dBm)", limits = c(-134, -50), breaks = seq(-130, 0, by = 10), minor_breaks = NULL) +
          theme_bw(base_size = base_size) + theme(panel.grid.major = element_line("grey85")) + ggtitle("Signal dominance")
  } else {
      ggplot(df, aes(x=dBm, y=rsig)) + geom_line() +
          scale_y_continuous("Signal dominance", limits = c(0, 1), breaks = qty_classes$breaks, minor_breaks = NULL) +
          scale_x_continuous("Signal strength (dBm)", limits = c(-130, -50), breaks = seq(-130, 0, by = 10), minor_breaks = NULL) +
          theme_bw(base_size = base_size) + theme(panel.grid.major = element_line("grey85")) + ggtitle("Signal dominance")
  }
}

#' @name radiation_plot
#' @rdname plot_functions
#' @param type \code{"a"} for azimuth (horizontal) plane and \code{"e"} for elevation/vertical plane
#' @param beam_width beam width
#' @param db_back difference in signal strength between the propagation direction of the cell and the opposite direction
#' @export
radiation_plot <- function(type = "a", beam_width, db_back = -30, base_size = 11) {
    deg <- dbLoss <- x <- y <- NULL
  start <- ifelse(type=="a", 0, pi/2)
  title <- ifelse(type=="a", "Azimuth Plane Pattern", "Elevation Plane Pattern")
  df <- data.frame(deg = seq(0, 360, length.out = 360))
  df$dbLoss <- norm_dBloss(a = df$deg, db_back = db_back, beam_width = beam_width)
  df2 <- data.frame(a = rep(0, 4), deg = c(0, -10, -20, -30))
  deg_dbmin3 <- round(df$deg[which.min(abs(-3 - df$dbLoss))])
  if (deg_dbmin3 > 180) deg_dbmin3 <- 360 - deg_dbmin3
  df3 <- data.frame(x = c(deg_dbmin3, 360 - deg_dbmin3), y = -3)

  brks <- seq(30, 330, by = 30)
  brks_diff <- pmin(abs(brks - df3$x[1]), abs(brks - df3$x[2]))

  if (min(brks_diff) < 5) {
    brks <- sort(c(brks[brks_diff>=5], df3$x))
  } else {
    brks <- sort(c(brks, df3$x))
  }

  ggplot(df, aes(x=deg, y= dbLoss)) +
    scale_x_continuous(title, breaks = brks) +
    geom_hline(yintercept = c(-30, -20, -10, 0), colour = "grey70", size = 0.4) +
    geom_vline(xintercept = seq(0, 330, by = 30), colour = "grey70", size = 0.4) +
      geom_line(size = 0.5) +
      geom_vline(xintercept = df3$x, colour = "red", size = 0.4) +
    geom_point(aes(x=x,y=y), data=df3, colour = "red", size = 1) +
    geom_text(aes(label = deg, x = a, y=deg-2), data = df2, size = 3, color = "grey20") +
    scale_y_continuous(limits = c(-35, 0)) +
    coord_polar(theta = "x", start=start) +
    theme_bw(base_size = base_size) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid=element_blank(),
          panel.border = element_blank())
}
