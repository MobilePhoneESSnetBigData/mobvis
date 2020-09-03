#' Interactive tool to setup the propagation model
#'
#' Tool to setup the propagation model parameters
#'
#' @param param parameter list created with \code{mobloc_param}
#' @param plot.height heigth of the plots in the tool (in pixels)
#' @import mobloc
#' @import shiny
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @example ./examples/setup_sig_strength_model.R
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
setup_sig_strength_model <- function(param = mobloc::mobloc_param(), plot.height=800) {


    SliderInput <- function(...) {
        div(style = "height: 75px;line-height:75%;", sliderInput(...))
    }

    app <- shinyApp(
        ui = fluidPage(
            #shinyjs::useShinyjs(),
            shiny::div(style = "font-size:75%;",
                       titlePanel("Signal strength model setup"),
                       fluidRow(
                           column(3,
                                  wellPanel(
                                      shiny::HTML("<h3>Cell configuration</h3>"),
                                      div(style = "font-size: 125%", checkboxInput("small", "Small cell", value = FALSE)),
                                      div(style = "font-size: 125%", checkboxInput("directional", "Directional", value = TRUE)),

                                      SliderInput("W", "Power (Watt)", 1, 30, value = param$W, step = .5),
                                      div(style = "margin-bottom:10px;", shiny::uiOutput("Wtext")),
                                      SliderInput("height", "Height", 10, 300, value = param$height, step = 5),
                                      SliderInput("tilt", "Tilt", 0, 20, value = param$tilt, step = 1),
                                      SliderInput("ple", "Path loss exponent", min = 1.5, max = 6, step = 0.25, value = param$ple),
                                      conditionalPanel("input.directional",
                                                       SliderInput("h3dB", "Horizontal -3dB angle", 20, 70, value = param$beam_h, step = 1),
                                                       SliderInput("hback", "Hozizontal dB back", -40, -10, value = param$azim_dB_back, step = 5),
                                                       SliderInput("v3dB", "Vertical -3dB angle", 4, 20, value = param$beam_v, step = 1),
                                                       SliderInput("vback", "Vertical dB back", -40, -10, value = param$elev_dB_back, step = 5)))
                                      # conditionalPanel("!input.small",
                                      #                  SliderInput("height", "Height", 10, 300, value = param$height, step = 5),
                                      #                  SliderInput("tilt", "Tilt", 0, 20, value = param$tilt, step = 1),
                                      #                  SliderInput("W", "Power (Watt)", 1, 30, value = param$W, step = .5),
                                      #                  div(style = "margin-bottom:10px;", shiny::uiOutput("Wtext")),
                                      #                  SliderInput("ple", "Path loss exponent", min = 1.5, max = 6, step = 0.25, value = param$ple),
                                      #                  SliderInput("h3dB", "Horizontal -3dB angle", 20, 70, value = param$beam_h, step = 1),
                                      #                  SliderInput("hback", "Hozizontal dB back", -40, -10, value = param$azim_dB_back, step = 5),
                                      #                  SliderInput("v3dB", "Vertical -3dB angle", 4, 20, value = param$beam_v, step = 1),
                                      #                  SliderInput("vback", "Vertical dB back", -40, -10, value = param$elev_dB_back, step = 5)),
                                      # conditionalPanel("input.small",
                                      #                  SliderInput("height_small", "Height", 10, 300, value = param$height_small, step = 5),
                                      #                  SliderInput("W_small", "Power (Watt)", 1, 30, value = param$W_small, step = .5),
                                      #                  div(style = "margin-bottom:10px;", shiny::uiOutput("Wtext_small")),
                                      #                  SliderInput("ple_small", "Path loss exponent", min = 1.5, max = 6, step = 0.1, value = param$ple_small)))
                           ),
                           column(3,
                                  wellPanel(
                                      shiny::HTML("<h3>Signal dominance configuration</h3>"),
                                      sliderInput("midpoint", "Midpoint", -120, -70, value = param$midpoint, step = 2.5),
                                      sliderInput("steepness", "Steepness", 0, 1, value = param$steepness, step = 0.05)),
                                  wellPanel(
                                      shiny::HTML("<h3>Heatmap setup</h3>"),
                                      radioButtons("type", "Output type", choices = c("Signal strength (dBm)" = "dBm", "Signal dominance" = "dominance"), selected = "dBm"),
                                      checkboxGroupInput("enable", "Signal loss components", choices = c("Distance" =  "d", "Horizontal offset" = "h", "Vertical offset" = "v"), selected = c("d", "h", "v")),
                                      sliderInput("range", "Heatmap range (m)", 250, 30000, value = 20000, step = 250, ticks = FALSE),
                                      # conditionalPanel("!input.small", sliderInput("range", "Heatmap range (m)", 250, 30000, value = 20000, step = 250, ticks = FALSE)),
                                      # conditionalPanel("input.small", sliderInput("range_small", "Heatmap range (m)", 250, 30000, value = 1000, step = 250, ticks = FALSE)),
                                      radioButtons("colors", "Color scale", choices = c("Discrete scale" = "discrete", "Gradient scale" = "gradient"), selected = "discrete"),
                                      conditionalPanel("input.colors == 'gradient'",
                                                       checkboxInput("mask", "Enable mask", value = FALSE),
                                                       conditionalPanel("input.mask && input.type == 'dBm'",
                                                                        sliderInput("maskrangedb", "Mask range", min = -130, max = -50, value = c(-100, -50))),
                                                       conditionalPanel("input.mask && input.type == 'dominance'",
                                                                        sliderInput("maskrangelh", "Mask range", min = 0, max = 1, value = c(.8, 1), step = .05))))
                           ),
                           column(6,
                                  plotOutput("heatmap", height=plot.height / 2),
                                  plotOutput("lines", height=plot.height / 4),
                                  plotOutput("radiation", height=plot.height / 4)
                           ))
            )),
        server = function(input, output, session) {

            observeEvent(input$small, {
                small <- input$small
                shiny::updateSliderInput(session, "height", value = ifelse(small, param$height_small, param$height))
                shiny::updateSliderInput(session, "W", value = ifelse(small, param$W_small, param$W))
                shiny::updateSliderInput(session, "ple", value = ifelse(small, param$ple_small, param$ple))
                shiny::updateSliderInput(session, "range", value = ifelse(small, 1000, 20000))
            })


            get_param_model <- reactive({
                list(height = input$height,
                     tilt = input$tilt,
                     direction = ifelse(input$directional, 90, NA),
                     W = input$W,
                     ple = input$ple,
                     midpoint = input$midpoint,
                     steepness = input$steepness,
                     h3dB = input$h3dB,
                     hback = input$hback,
                     v3dB = input$v3dB,
                     vback = input$vback)

                # if (input$small) {
                #     list(height = input$height_small,
                #          direction = NA,
                #          W = input$W_small,
                #          ple = input$ple_small,
                #          midpoint = input$midpoint,
                #          steepness = input$steepness)
                # } else {
                #     list(height = input$height,
                #          tilt = input$tilt,
                #          direction = 90,
                #          W = input$W,
                #          ple = input$ple,
                #          midpoint = input$midpoint,
                #          steepness = input$steepness,
                #          h3dB = input$h3dB,
                #          hback = input$hback,
                #          v3dB = input$v3dB,
                #          vback = input$vback)
                # }
            })

            get_param_plots <- reactive({
                list(type = input$type,
                     enable = if (!input$directional) "d" else input$enable,
                     range = input$range, #ifelse(input$small, input$range_small, input$range),
                     mask = input$mask,
                     maskrangedb = input$maskrangedb,
                     maskrangelh = input$maskrangelh,
                     colors = input$colors)
            })


            output$Wtext <- renderUI({
                W <- get_param_model()$W
                dBm <- W2dBm(W)
                dBW <- W2dBW(W)
                shiny::HTML(sprintf("%.1f", W), " W = ", round(dBW), " dBW = ", round(dBm), "dBm")
            })

            # output$Wtext_small <- renderUI({
            #     W <- get_param_model()$W
            #     dBm <- W2dBm(W)
            #     dBW <- W2dBW(W)
            #     shiny::HTML(sprintf("%.1f", W), " W = ", round(dBW), " dBW = ", round(dBm), "dBm")
            # })

            output$heatmap <- renderPlot({
                param_model <- get_param_model()
                param_plots <- get_param_plots()
                heatmap_ground(param_model, param_plots, param)
            })


            output$lines <- renderPlot({
                param_model <- get_param_model()
                param_plots <- get_param_plots()
                g1 <- distance_plot(W = param_model$W, ple = param_model$ple, range = param_plots$range)

                g2 <- signal_dominance_plot(midpoint = param_model$midpoint, steepness = param_model$steepness)

                grid.arrange(g1, g2, ncol = 2)
            })


            output$radiation <- renderPlot({
                param_model <- get_param_model()
                if (!is.na(param_model$direction)) {
                    g3 <- radiation_plot(type = "a", db_back = param_model$hback, beam_width = param_model$h3dB)
                    g4 <- radiation_plot(type = "e", db_back = param_model$vback, beam_width = param_model$v3dB)

                    grid.arrange(g3, g4, ncol = 2)
                } else {
                    NULL
                }

            })

        }
    )
    runApp(app)
}
