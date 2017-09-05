

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
tm_splom <- function(label) {

  module(
    label = label
  )


}

ui_splom <- function(id) {

  ns <- NS(id)

   standard_layout(
     output = plotOutput(ns("plot_ui")),
     encoding = div(
       tags$label("Encodings", class="text-primary"),
       selectInput(ns("biomarker"), "Biomarker", choices=choices, selected = head(choices, 5),
                   multiple = TRUE),
       tags$label("Plot Settings", class="text-primary"),
       sliderInput(ns("plot_height"), "Plot Height", min=400, max=3000, step = 10, value = 800),
       sliderInput(ns("alpha"), "Transparency", min=0, max=1, step = .05, value = .5),
       sliderInput(ns("cex"), "Point Size", min=0.2, max=3, step = .05, value = .65)
     ),
     pre_output = helpText("This scatterplot maxtrix shows the expression data <code>ABM2.AVAL</code> for the different biomarkers <code>ABM2.PARAM</code>.")
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
