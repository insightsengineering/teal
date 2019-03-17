#' Made Up Plot module
#' 
#' @import teal
#' @export
#' @param label character
#' @param dataname character
#' @param response (\code{data_extract})
#' @param regressor (\code{data_extract})
#' @param facetting (\code{data_extract})
#' @inheritParams teal::standard_layout
#' 
tm_made_up <- function(
                       label = "Regression Analysis",
                       dataname = NULL,
                       response,
                       regressor,
                       facetting = NULL,
                       pre_output = NULL,
                       post_output = NULL) {
  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_made_up,
    ui = ui_made_up,
    ui_args = args,
    server_args = list(regressor = regressor, response = response, facetting = facetting),
    filters = "all"
  )
}

#' @importFrom teal.devel plot_height_output plot_height_input plot_with_height
ui_made_up <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)

  # layout
  standard_layout(
    output = teal.devel::white_small_well(
      tags$div(
        # This shall be wrapped in a teal::plot
        teal.devel::plot_height_output(id = ns("outplot")),
        shiny::textOutput(ns("outtext")),
        shiny::textOutput(ns("outtext_response"))
      )
    ),
    encoding = div(
      data_extract_input(
        id = ns("regressor"),
        label = "Regressor Variable",
        value = arguments$regressor
      ),
      data_extract_input(
        id = ns("response"),
        label = "Response Variable",
        value = arguments$response
      ),
      data_extract_input(
        id = ns("facetting"),
        label = "Facetting Variable",
        value = arguments$facetting
      ),
      teal.devel::plot_height_input(id = ns("height"))
      # This shall be wrapped in a teal::plot
    )
  ) # standard_layout
}

#' @importFrom ggplot2 ggplot geom_point aes_string
srv_made_up <- function(input, output, session, datasets, response, regressor, facetting) {

  # data_extractor, "response",
  # dataname AND filtering (yes/no) AND Names(Filtering-selected) AND Names(Columns-Selected)

  regressor_data <- callModule(data_extractor, id = "regressor", datasets = datasets, constant_values = regressor)
  response_data <- callModule(data_extractor, id = "response", datasets = datasets, constant_values = response)
  facetting_data <- callModule(data_extractor, id = "facetting", datasets = datasets, constant_values = facetting)

  output$plot <- renderPlot({

    #  plot(lm(
    #      as.formula(paste(get_selected_columns(response_data()), paste(get_selected_columns(regressor_data()), collapse=" + "), sep=" ~ "))
    #  , data = extracted_data(regressor_data(), response_data())))

    ggplot(
      data = extracted_data(regressor_data(), response_data()),
      aes_string(
        x = get_selected_columns(regressor_data()),
        y = get_selected_columns(response_data())
      )
    ) + geom_point()
  })
  callModule(teal.devel::plot_with_height, id = "outplot", plot_id = session$ns("plot"), plot_height = reactive(input$height))

  output$outtext <- shiny::renderText({
    get_selected_columns(regressor_data())
  })
  output$outtext_response <- shiny::renderText({
    get_selected_columns(response_data())
  })
}
