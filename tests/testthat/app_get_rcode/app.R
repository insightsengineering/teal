# nolint start # because app$setInput etc. are not well chosen names

#---------------------------------------------------------------------------------------
#------------------ Module Code --------------------------------------------------------
#---------------------------------------------------------------------------------------
source("../helper-functions.R")
eval(shinytest_load_pckg("teal"))

# Made Up linear model module
tm_made_up_lm <- function(label = "Regression Analysis",
                          dataname = NULL,
                          response,
                          regressor,
                          pre_output = NULL,
                          post_output = NULL) {
  args <- as.list(environment())

  module(
    label = label,
    server = srv_made_up_lm,
    ui = ui_made_up_lm,
    ui_args = args,
    server_args = list(regressor = regressor, response = response, dataname = dataname),
    filters = "all"
  )
}

ui_made_up_lm <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)

  # layout
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      tags$div(
        # This shall be wrapped in a teal::plot
        teal.widgets::plot_with_settings_ui(id = ns("outplot")),
        textOutput(ns("outtext")),
        textOutput(ns("outtext_response")),
        verbatimTextOutput(ns("formula")),
        verbatimTextOutput(ns("model"))
      )
    ),
    encoding = div(
      teal.transform::data_extract_ui(
        id = ns("regressor"),
        label = "Regressor Variable",
        data_extract_spec = arguments$regressor
      ),
      teal.transform::data_extract_ui(
        id = ns("response"),
        label = "Response Variable",
        data_extract_spec = arguments$response
      )
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
  )
}

srv_made_up_lm <- function(id, datasets, response, regressor, dataname) {
  moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()
    # data_extract_srv, "response",
    # dataname AND filtering (yes/no) AND Names(Filtering-selected) AND Names(Columns-Selected)

    merged_data <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(regressor = regressor, response = response)
    )

    model <- reactive({
      teal.code::chunks_reset()
      teal.code::chunks_push_data_merge(merged_data())

      validate_has_data(chunks_get_var("ANL"), min_nrow = 3)

      teal.code::chunks_push(expression = rlang::expr(5 + 5))
      teal.code::chunks_push_comment(comment = "")
      teal.code::chunks_push_comment(
        comment = "I am testing an addition
        what can be done
        "
      ) # Just first line is shown in output.
      teal.code::chunks_push(expression = quote(3 + 3))
      lm(
        formula = stats::as.formula(
          paste(
            merged_data()$columns_source$response,
            paste(
              merged_data()$columns_source$regressor,
              collapse = " + "
            ),
            sep = " ~ "
          )
        ),
        data = merged_data()$data()
      )
    })

    plot_r <- reactive({
      plot(model(), which = 1)
    })
    teal.widgets::plot_with_settings_srv(id = "outplot", plot_r = plot_r)

    output$outtext <- renderText({
      merged_data()$columns_source$regressor
    })
    output$outtext_response <- renderText({
      merged_data()$columns_source$response
    })
    output$formula <- renderText({
      paste(
        merged_data()$columns_source$response,
        paste(
          merged_data()$columns_source$regressor,
          collapse = " + "
        ),
        sep = " ~ "
      )
    })
    output$model <- renderPrint({
      summary(model())
    })

    observeEvent(input$show_rcode, {
      show_rcode_modal(
        title = "R Code for a Regression Plot",
        rcode = get_rcode(
          datasets = datasets,
          title = "R Code for a Regression Plot",
          description = ""
        )
      )
    })
  })
}

#---------------------------------------------------------------------------------------
#------------------ App Code -----------------------------------------------------------
#---------------------------------------------------------------------------------------
library(scda)
library(teal)
library(magrittr)

ADSL <- synthetic_cdisc_data("rcd_2021_05_05")$adsl
ADTTE <- synthetic_cdisc_data("rcd_2021_05_05")$adtte

adtte_filters <- filter_spec(
  vars = "PARAMCD", # only key variables are allowed
  sep = " - ",
  choices = value_choices(ADTTE, var_choices = "PARAMCD", var_label = "PARAM"),
  selected = "OS",
  multiple = TRUE, # if multiple, then a pivot_wider is needed
  label = "Choose endpoint"
)


adtte_extracted1 <- data_extract_spec(
  dataname = "ADTTE",
  filter = adtte_filters,
  select = select_spec(
    choices = variable_choices(ADTTE, subset = c("AVAL", "AVALU", "BMRKR1", "SITEID")),
    selected = "AVAL",
    multiple = TRUE,
    fixed = FALSE, # Whether the user can select the item (optional)
    label = "Column" # Label the column select dropdown (optional)
  )
)

adtte_extracted <- data_extract_spec(
  dataname = "ADTTE",
  filter = adtte_filters,
  select = select_spec(
    choices = variable_choices(ADTTE, subset = c("AVAL", "BMRKR1")),
    selected = "AVAL",
    multiple = FALSE,
    fixed = FALSE, # Whether the user can select the item
    label = "" # Label the column select dropdown (optional)
  )
)

adsl_extracted <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = variable_choices(ADSL, subset = c("SEX", "AGE")),
    selected = "AGE",
    multiple = TRUE,
    fixed = FALSE
  )
)

x <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL", ADSL),
    cdisc_dataset("ADTTE", ADTTE),
    code = "",
    check = FALSE
  ),
  modules = modules(
    tm_made_up_lm(
      label = "Regression",
      dataname = c("ADSL", "ADTTE"),
      response = adtte_extracted,
      regressor = list(
        adtte_extracted1,
        adsl_extracted
      )
    )
  )
)

runApp(x) # nolint end
