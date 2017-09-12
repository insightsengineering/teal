

#' Scatterplot Matrix Teal Module
#'
#' Accepts Vertical and Horizontal Data
#'
#'
#'
#' @examples
#' anl_hor <- data.frame(
#'   PARAMCD = rep(letters[1:5], each = 40),
#'   AVAL = rnorm(40*5)
#' )
#'
#'
#' anl_ver <- as.data.frame(replicate(5, rnorm(40), simplify=FALSE))
#' names(anl_ver) <- letters[1:5]
#'
#' anl_ver
#'
#'
tm_splom_wide_data <- function(label) {

  module(
    label = label
  )

}


tm_splot_long_data <- function(label) {

}

ui_splom <- function(id, label_select_var = "Variable", choices, selected = head(choices, 5),
                     plot_height = c(800, 400, 3000),
                     alpha = c(0.5, 0, 1),
                     point_size = c(.65, .2, 3),
                     pre_output,
                     post_output
                     ) {

  ns <- NS(id)

   standard_layout(
     output = plotOutput(ns("plot_ui")),
     encoding = div(
       tags$label("Encodings", class="text-primary"),
       selectInput(ns("splom_vars"), label_select_var, choices=choices, selected = selected,
                   multiple = TRUE),
       optionalSelectInput(ns("color_by")),
       tags$label("Plot Settings", class="text-primary"),
       sliderInput(ns("plot_height"), "Plot Height", min=plot_height[2], max=plot_height[3], value = plot_height[1]),
       sliderInput(ns("alpha"), "Transparency", min=alpha[2], max=alpha[3], step = .05, value = alpha[1]),
       sliderInput(ns("cex"), "Point Size", min=point_size[2], max=point_size[3], step = .05, value = point_size[1])
     ),
     pre_output = pre_output,
     post_output = post_output
   )
}

srv_splom <- function(input, output, session, datasets) {

  ## dynamic plot height
  output$plot_ui <- renderUI({

    plot_height <- input$plot_height

    validate(need(plot_height, "need valid plot height"))

    ns <- session$ns
    plotOutput(ns("plot"), height=plot_height)
  })

  anl <- reactive({
    ABM2 <- datasets$get_data("ABM2", filtered = TRUE, reactive = TRUE)
    biomarkers <- input$biomarker

    validate(
      need(biomarkers, "select biomarkers"),
      need(length(unique(ABM2$VISIT)) == 1, "filter for a single ABM2.VISIT")
    )

    df <- ABM2  %>% select(USUBJID, PARAM, AVAL) %>% filter(PARAM %in% biomarkers)

    anl <- reshape(as.data.frame(df), idvar = "USUBJID", timevar = "PARAM", direction = "wide")
    names(anl) <- sub("^AVAL\\.", "", names(anl))

    anl
  })

  output$plot <- renderPlot({

    anl <- anl()
    alpha <- input$alpha
    cex <- input$cex

    validate(
      need(alpha, "need valid transparency value"),
      need(cex, "need valid point size")
    )


    # head(x)

    lattice::splom(anl[, -1], alpha = alpha, pch = 16, cex = cex)

  })
}
