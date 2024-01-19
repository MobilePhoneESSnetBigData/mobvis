
raster2bbx <- function(raster) {
    crs <- raster@crs@projargs
    ex <- extent(raster)
    st_bbox(c(xmin = ex[1], ymin = ex[3], xmax = ex[2], ymax = ex[4]), crs = crs)
}

prior_to_df <- function(prior, raster) {
    data.table(rid = raster[],
               p = prior[])[!is.na(p)]
}

#' Explore the visualize propagation, prior, connection likelihood and location posterior probabilities per raster tile
#'
#' Explore the visualize propagation, prior, connection likelihood and location posterior probabilities per raster tile. When the raster is large (say larger than 30 by 30 kilometers), we recommend to specify the filter arguemnt.
#'
#' @param cp cellplan, validated with \code{\link{validate_cellplan}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param strength a signal strength model object, which is the result of \code{\link{compute_sig_strength}}
#' @param priorlist list of priors
#' @param llhlist list of likelihoods
#' @param param parameter list created with \code{mobloc_param}
#' @param filter bounding box of the filter of the visualized raster. If not specified, the whole raster is shown, which could be very slow. Therefore, we recommand to use a filter when the raster covers a large area (say 30 by 30 kilometers).
#' @param coverage_map_dBm coverage map, created with \code{\link{create_coverage_map}} (with \code{type = "dBm"}). If not specified, it will be created (which takes some time).
#' @param coverage_map_s coverage map, created with \code{\link{create_coverage_map}} (with \code{type = "s"}). If not specified, it will be created (which takes some time).
#' @param best_server_map best server map, created with \code{\link{create_best_server_map}}. If not specified, it will be created (which takes some time).
#' @param settings mobvis settings, see \code{\link{mobvis_settings}}
#' @param height height of the map in pixels
#' @note Note that duo to the reprojection of the raster to the web mercator projection (for interactive maps), the visualized raster does not correspond exactly to the output raster.
#' @import shiny
#' @importFrom shinyjs useShinyjs disable
#' @import tmap
#' @import viridisLite
#' @import sf
#' @importFrom dplyr rename select filter mutate left_join
#' @importFrom tidyr unnest_wider unnest pivot_longer starts_with
#' @import readr
#' @importFrom raster raster setValues brick extent crop extract coordinates
#' @import data.table
#' @example ./examples/explore_mobloc.R
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
explore_mobloc <- function(cp, raster, strength, priorlist, llhlist, param, filter = NULL, coverage_map_dBm = NULL, coverage_map_s = NULL, best_server_map = NULL, settings = mobvis_settings_interactive(), height = 600) {


    if (!missing(filter)) {

        raster <- mobloc_crop_raster(raster, bbx = filter)

        a <- local({
            cells <- do.call(c, lapply(llhlist, function(llh) llh$cell))
            rids <- do.call(c, lapply(llhlist, function(llh) llh$rid))
            cp$cell[unique(cells[rids %in% raster[]])]
        })
        cp <- mobloc_filter_cell(cp, a)

        priorlist <- lapply(priorlist, mobloc_crop_raster, bbx = filter)
        llhlist <- lapply(llhlist, mobloc_filter_cell, a = a)
    }


    rect <- st_set_crs(create_bbx_rect(raster2bbx(raster)), st_crs(raster)) %>% st_transform(crs = 4326)



    cell <- NULL

    pnames <- c(names(priorlist), "composite")
    nprior <- length(pnames)
    choices_prior <- paste0("p", 1L:nprior)
    names(choices_prior) <- paste0("Prior ", pnames)
    names(pnames) <- choices_prior

    lnames <- names(llhlist)
    nllh <- length(lnames)
    choices_llh <- paste0("l", 1L:nllh)
    names(choices_llh) <- paste0("Likelihood ", lnames)
    names(lnames) <- choices_llh


    choices <- c("None" = "none",
                 "Signal strength - dBm" = "dBm",
                 "Signal dominance - s" = "s",
                 "Best server map" = "bsm",
                 "Location prior - P(g)" = "pg",
                 "Connection likelihood - P(a|g)" = "pag",
                 "Location posterior - P(g|a)" = "pga")


    prob_th_na = is.na(settings$prob_th)
    prob_th = ifelse(prob_th_na, 1e-15, settings$prob_th)

    #https://stackoverflow.com/questions/34733147/unable-to-disable-a-shiny-app-radio-button-using-shinyjs

    cells <- as.character(cp$cell)
    #names(cells) <- paste("Cell", 1L:n)


    if (missing(coverage_map_dBm)) {
        message("Creating coverage maps (dBm)...")
        coverage_map_dBm <- create_coverage_map(strength, raster, type = "dBm") # cm_dBm
    }
    if (missing(coverage_map_s)) {
        message("Creating coverage maps (s)...")
        coverage_map_s <- create_coverage_map(strength, raster, type = "s") #cm_s
    }
    if (missing(best_server_map)) {
        message("Creating best server maps...")
        best_server_maps <- lapply(llhlist, create_best_server_map, raster = raster)
        #best_server_map <- create_best_server_map(strength, raster) #bsm
    }



    sliders <- mapply(function(i, nm) {
        if (i == choices_prior[nprior - 1]) {
            shiny::htmlOutput("plast")
        } else {
            sliderInput(i, paste("Faction", nm), min = 0, max = 1, value = 1/nprior, step  = 0.01)
        }

    }, choices_prior[-nprior], pnames[-nprior], SIMPLIFY = FALSE)

    app <- shinyApp(
        ui = fluidPage(
            useShinyjs(),
            tags$head(
                tags$style(HTML("
                  .disabled {
                    opacity: 0.4;
                  }
                "))
            ),
            titlePanel("Mobile location exploration"),
            sidebarLayout(
                sidebarPanel(
                    tabsetPanel(
                        tabPanel("Setup",
                                 fluidRow(
                                     column(6,
                                            h3("Map Setup"),
                                            radioButtons("show", "Selection",  c("All cells" = "grid", "Single cell" = "ant"), selected = "grid"),
                                            radioButtons("var", "Show", choices, selected = "none"),
                                            sliderInput("trans", "Transparency", min = 0, max = 1, value = 1, step = 0.1),
                                            checkboxInput("prob0", "Show probality = 0", value = prob_th_na)),
                                     column(6,
                                            h3("Module Setup"),
                                            radioButtons("varP", "Location Prior", choices_prior, selected = "p1"),
                                            conditionalPanel(condition = paste0("(input.varP == 'p", nprior, "')"),
                                                             wellPanel(sliders)),
                                            radioButtons("varL", "Connection Likelihood", choices_llh, selected = "l1"),
                                            HTML("<b>Timing Advance</b>"),
                                            checkboxInput("TA", "Enable Timing Advance", value = FALSE),
                                            conditionalPanel(
                                                condition = "input.TA",
                                                sliderInput("TAvalue", "Value", min = 0, max = param$TA_max, value = 0, step = 1),
                                                HTML(paste0("<span style='font-size:70%'>Each step corresponds to ", param$TA_step, " m</span>")),
                                                shiny::htmlOutput("TAband")
                                            )))),
                        tabPanel("Cell data",
                                 selectInput("sel", "Cell", cells, selected = cells[1]),
                                 dataTableOutput("cellinfo"))
                    )),
                mainPanel(
                    tmapOutput("map", height=height)
                ))
        ),
        server = function(input, output, session) {
            get_var <- reactive({
                show <- input$show
                var <- input$var
                if (show == "grid" && var %in% c("pag", "pga")) choices[1] else var
            })

            get_settings <- reactive({
                settings$prob_th = ifelse(input$prob0, NA, prob_th)
                settings
            })


            observe({
                show <- input$show
                var <- get_var()
                if (show == "grid") {
                    if (var != input$var) updateRadioButtons(session, "var", choices = choices, selected = var)
                    shinyjs::runjs("$('#var input[value=pag]').parent().parent().addClass('disabled')")
                    shinyjs::runjs("$('#var input[value=pga]').parent().parent().addClass('disabled')")
                } else {
                    shinyjs::runjs("$('#var input[value=pag]').parent().parent().removeClass('disabled')")
                    shinyjs::runjs("$('#var input[value=pga]').parent().parent().removeClass('disabled')")
                }
            })

            get_composition <- reactive({
                varP <- input$varP
                if (varP != paste0("p", nprior)) {
                    composition <- rep(0, nprior-1)
                    composition[as.integer(substr(varP, 2, nchar(varP)))] <- 1
                    attr(composition, "showW") <- FALSE
                } else {
                    values <- sapply(choices_prior[1:(nprior-2)], function(x) {
                        input[[x]]
                    })
                    if (sum(values) > 1) {
                        showW <- TRUE
                        values <- values / sum(values)
                    } else {
                        showW <- FALSE
                    }
                    composition <- c(values, 1-sum(values))
                    attr(composition, "showW") <- showW
                }
                composition
            })

            get_prior <- reactive({
                composition <- get_composition()
                do.call(create_prior, c(unname(priorlist), list(name = "composite", weights = composition)))
            })


            get_llh <- reactive({
                varL <- input$varL
                llhlist[[as.integer(substr(varL, 2, nchar(varL)))]]
            })

            get_bsm <- reactive({
                varL <- input$varL
                best_server_maps[[as.integer(substr(varL, 2, nchar(varL)))]]
            })


            output$plast <- renderUI({
                composition <- get_composition()
                showW <- attr(composition, "showW")
                HTML(paste0("<b>Faction ", pnames[nprior-1], ": ", round(composition[nprior-1], 2), ifelse(showW, " (warning: the sum of slider values is greater than 1)", ""),  "</b>"))
            })

            output$TAband <- renderUI({
                TA  <- input$TAvalue
                TA_buffer <- param$TA_buffer
                TA_step <- param$TA_step

                TA_min <- TA * TA_step
                TA_max <- (TA+1) * TA_step


                TA_min_band <- (max(0, TA-TA_buffer)) * TA_step
                TA_max_band <- (TA+TA_buffer+1) * TA_step

                if (TA_buffer > 0) {
                    HTML(paste0("<span style='font-size:70%'>TA band (with buffer): [", fN(TA_min_band), ", ", fN(TA_max_band), "] m<br>TA band (w.o. buffer): [", fN(TA_min), ", ", fN(TA_max), "] m</span>"))
                } else {
                    HTML(paste0("<span style='font-size:70%'>TA band: [", fN(TA_min), ", ", fN(TA_max), "] m</span>"))
                }

                #HTML(paste0("<b>Faction ", pnames[nprior], ": ", round(composition[nprior], 2), ifelse(showW, " (warning: the sum of slider values is greater than 1)", ""),  "</b>"))
            })



            # output$map <- renderLeaflet({
            #     base_map(cp, offset_value, epsg)
            # })


            output$map <- renderTmap({
                #map_mob_cells(cp, rst, var = type, offset = ifelse(input$offset, offset_value, 0), borders = rect, proxy = FALSE, interactive = TRUE, opacity = input$trans, basemaps = "OpenStreetMap", cells = sel)
                #browser()
                #tm_shape(cp) + tm_dots()
                base_tmap(cp, rect, basemaps = "OpenStreetMap", settings=settings)
            })


            output$cellinfo <- renderDataTable({
                cpant <- as.list(cp[cp$cell == input$sel, ] %>% st_set_geometry(NULL))
                cpant$x <- sprintf("%.2f", cpant$x)
                cpant$y <- sprintf("%.2f", cpant$y)
                cpant$z <- sprintf("%.2f", cpant$z)
                cpant$ple <- sprintf("%.2f", cpant$ple)
                data.frame(Variable = names(cpant), Value = unname(unlist(cpant)))
            }, options = list(searching = FALSE, scrollx = FALSE, paging = FALSE, info = FALSE))

            observe({
                type <- get_var()
                settings <- get_settings()
                sel <- input$sel
                cp$sel <- 1L
                cp$sel[cp$cell %in% sel] <- 2L

                if (type %in% c("bsm", "pag", "pga")) llh <- get_llh() else llh <- NULL
                if (type == "bsm") {
                    bsm <- get_bsm()
                } else {
                    bsm <- NULL
                }
                if (type %in% c("pg", "pga")) prior <- get_prior() else prior <- NULL

                if (input$show == "grid" && type == "none") {
                    rst <- NULL
                } else if (input$show == "grid") {
                    rst <- create_q_raster(raster, type = type, prior = prior, coverage_map_dBm, coverage_map_s, bsm, settings = settings)
                } else {
                    if (type %in% c("bsm", "pg", "pag", "pga")) {
                        llh <- get_llh()
                    }

                    ta <- NA
                    if (type == "pga") {
                        if (input$TA) {
                            ta <- input$TAvalue
                        }
                    }

                    if (type == "bsm") {
                        rst <- create_best_server_map(llh, raster, cells = sel)
                    } else {
                        if (type %in% c("dBm", "s")) {
                            dt <- strength[cell == sel]
                        } else {
                            dt <- llh[cell == sel]
                        }
                        cpsel <- cp[cp$sel == 2L, ]
                        rst <- create_p_raster(raster, dt, type = type, prior = prior, ta, param, cpsel, settings = settings)
                    }
                }
                map_mob_cells(cp, rst, var = type, region = rect, proxy = TRUE, interactive = TRUE, opacity = input$trans, cells = sel, settings = settings)

            })

            observeEvent(input$map_marker_click, { # update the location selectInput on map clicks
                p <- input$map_marker_click
                id <- which(sapply(cells, function(cl) {
                    length(grep(cl, p$id, fixed = TRUE)) == 1
                }))[1]


                if (length(id)!=0) {
                    updateSelectInput(session, "sel",
                                      selected = cells[id])
                }

            })


        }
    )

    suppressWarnings(runApp(app)) # to suppress: Ignoring appended content; appendContent can't be used in a Shiny render call
}


create_q_raster <- function(rst, type, prior, cm_dBm, cm_s, bsm, settings) {
    #rindex <- raster::getValues(rst)
    #r <- raster::raster(rst)

    if (type == "dBm") {
        r <- cm_dBm
    } else if (type == "s") {
        r <- cm_s
    } else if (type == "bsm") {
        r <- bsm
    } else if (type == "pg") {
        #composition <- c(priormix[1], (priormix[2] - priormix[1]), (1 - priormix[2]))
        r <- prior
    }

    if (!is.na(settings$prob_th)) raster::trim(r) else r
}

create_p_raster <- function(rst, dt, type, prior, ta, param, cpsel, settings) {
    dBm <- s <- pag <- pg <- pga <- TA <- x <- rid <- NULL

    rindex <- raster::getValues(rst)
    r <- raster::raster(rst)

    if (type == "none") {
        return(r)
    } else if (type == "dBm") {
        dt[, x:= dBm]
    } else if (type == "s") {
        dt[, x:=s]
    } else if (type == "pg") {
        priordf <- prior_to_df(prior, rst)
        dt[, x:= priordf$p[match(dt$rid, priordf$rid)]]
    } else if (type == "pag") {
        dt[, x := pag]
    } else  if (type == "p") {
        dt[, x := p]
    } else {

        if (!("pga" %in% names(dt))) {
            priordf <- prior_to_df(prior, rst)
            dt[, p:= priordf$p[match(dt$rid, priordf$rid)]]

            #setnames(dt, "pg", "p")

            dt <- calculate_posterior(prior, dt, rst)
            #dt <- calculate_mobloc(dt, timing.advance = !is.na(ta), param = param)

            if (!is.na(ta)) {
                dt <- update_posterior_TA(dt, raster = rst, cp = cpsel, param = param)[TA==ta, ]
            }
        }

        setnames(dt, "pga", "x")


        #if (!is.na(ta)) browser()

        if (nrow(dt) == 0) {
            return(r)
        }
    }

    dt <- dt[rid %in% rindex, ]

    if (nrow(dt)==0) return(r)

    raster::values(r)[match(dt$rid, rindex)] <- dt$x
    if (!is.na(settings$prob_th)) r <- raster::trim(r)

    r
}


fN <- function(x) {
    formatC(x, big.mark = ",", format = "f", digits = 0)
}
