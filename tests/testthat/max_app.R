# library(devtools)
# install_github(
#   'NEST/random.cdisc.data',
#   ref = "devel",
#   host = 'https://github.roche.com/api/v3',
#   upgrade_dependencies = FALSE,
#   build_vignettes = TRUE
# )
# install_github(
#   'NEST/teal',
#   ref = "devel",
#   host = 'https://github.roche.com/api/v3',
#   upgrade_dependencies = FALSE,
#   build_vignettes = TRUE
# )
# teal.devel must be public repo to install
# install_github(
#   'NEST/teal.devel',
#   ref = "devel",
#   host = 'https://github.roche.com/api/v3',
#   upgrade_dependencies = FALSE,
#   build_vignettes = TRUE
# )
library(random.cdisc.data)
ADSL_raw <- radsl()  # horizontal: one row per patient
ADLB_raw <- radlb(ADSL_raw) # vertical: multiple rows per patient: keys = USUBJID, STUDYID, PARAMCD, AVISIT

# to be used like a class
Dataset <- function(name, raw_data, keys) {
  #todo: replace raw_data by data_name for use with teal, write function to get raw data given data name
  list(name=name, raw_data=raw_data, keys=keys)
}

ADSL <- Dataset(name="ADSL", raw_data=ADSL_raw, keys=c("STUDYID", "USUBJID"))
ADLB <- Dataset(name="ADLB", raw_data=ADLB_raw, keys=c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")) #todo: error in doc: AVISIT instead of AVISIT


DatasetViewSpec <- function(dataset, view_name, keys_filter_spec, columns_filter_spec) {
  stopifnot(!is.null(columns_filter_spec))
  list(dataset=dataset, view_name=view_name, keys_filter_spec=keys_filter_spec, columns_filter_spec=columns_filter_spec)
}

# SingleDatasetView
DatasetView <- function(dataset, view_name, keys_filter, columns_filter) {
  stopifnot(!is.null(columns_filter)) # if columns_filter is NULL, does not take any columns: does not make sense
  if (!is.null(keys_filter)) {
    stopifnot(is.list(keys_filter$selected))
    stopifnot("keys" %in% attributes(keys_filter)$names)
  }
  if (!is.null(columns_filter)) {
    stopifnot(is.atomic(columns_filter$selected))
  }
  list(dataset=dataset, view_name=view_name, keys_filter=keys_filter, columns_filter=columns_filter)
}

ADSL_view <- DatasetView(ADSL, view_name="adsl", keys_filter=NULL, columns_filter=list(selected=c("AGE", "SEX")))
ADLB_view <- DatasetView(ADLB, view_name="adlb", keys_filter=list(selected=list(c("CRP", "BASELINE")), keys=c("PARAMCD", "AVISIT")), columns_filter=list(selected=c("AVAL", "AVALU")))
# ADSL_view <- DatasetView(ADSL, view_name="adsl", keys_filter=NULL, columns_filter=list(selected=c("AGE")))
# ADLB_view <- DatasetView(ADLB, view_name="adlb", keys_filter=list(selected=list(c("CRP", "BASELINE")), keys=c("PARAMCD", "AVISIT")), columns_filter=list(selected=c("AVAL")))

ColumnsChoice <- function(choices, default_choice, multiple, ...) {
  stopifnot(is.atomic(choices))
  stopifnot(is.atomic(default_choice))
  stopifnot(multiple || length(default_choice) == 1)
  list(choices=choices, default_choice=default_choice, multiple=multiple, enabled=TRUE, ...)
}
ColumnsChoice(c("AGE", "SEX"), c("SEX", "AGE"), multiple=TRUE, show=TRUE)
ColumnsChoice(c("AGE", "SEX"), "AGE", multiple=FALSE, show=TRUE)


KeysChoiceFromCombinations <- function(choices, default_choice, multiple, sep, keys, ...) {
  stopifnot(is.atomic(choices))
  stopifnot(is.atomic(default_choice))
  stopifnot(multiple || length(default_choice) == 1)
  
  split_by_sep <- function(x) {
    stopifnot(is.atomic(x))
    res <- strsplit(x, sep, fixed=TRUE)
    stopifnot(all(vapply(res, function(splitted_str) length(splitted_str) == length(keys), TRUE)))
    res
  }
  #reverse of split_by_sep
  merge_by_sep <- function(x) {
    stopifnot(is.list(x))
    lapply(x, function(elem) paste(elem, collapse=sep))
  }
  choices <- split_by_sep(choices)
  default_choice <- split_by_sep(default_choice)
  stopifnot(all(vapply(choices, function(choice) length(choice) == length(keys), TRUE)))
  stopifnot(all(vapply(default_choice, function(choice) length(choice) == length(keys), TRUE)))
  list(choices=choices, default_choice=default_choice, multiple=multiple, keys=keys, value_to_label=merge_by_sep, label_to_value=split_by_sep, ...)
}

KeysChoiceFromCombinations(c("CRP - BASELINE", "CRP - SCREENING", "ALT - BASELINE"), "CRP - BASELINE", multiple=TRUE, sep=" - ", keys=c("PARAMCD", "AVISIT"), show=TRUE)


ADSL_viewspec <- DatasetViewSpec(ADSL,
    view_name="adsl",
    keys_filter_spec=NULL,
    columns_filter_spec=ColumnsChoice(c("AGE", "SEX"), "AGE", multiple=FALSE, show=TRUE))
ADLB_viewspec <- DatasetViewSpec(ADLB,
    view_name="adlb",
    keys_filter_spec=KeysChoiceFromCombinations(c("CRP - BASELINE", "CRP - SCREENING", "ALT - BASELINE"), "CRP - BASELINE", multiple=TRUE, sep=" - ", keys=c("PARAMCD", "AVISIT"), show=TRUE),
    columns_filter_spec=ColumnsChoice(c("AVAL", "AVALU"), "AVAL", multiple=TRUE, show=TRUE))


get_keysfilter_str <- function(keys_filter) {
  if (is.null(keys_filter)) {
    "TRUE"
  } else {
    keys <- keys_filter$keys
    paste0(lapply(keys_filter$selected, function(keys_values) {
              # keys_values <- keys_filter$selected[[1]]
              paste0(Map(function(key_name, key_value) paste0(key_name, " == '", key_value, "'"), keys, keys_values), collapse=" & ")
            }), collapse=" | ")
  }
}
get_keysfilter_str(ADLB_view$keys_filter)

apply_dataview <- function(dataview) {
  # returns datasets with columns filtered and keys filtered
  full_dataset <- dataview$dataset
  keys_to_remove <- dataview$keys_filter$keys
  stopifnot(all(keys_to_remove %in% full_dataset$keys))
  new_keys <- setdiff(full_dataset$keys, keys_to_remove)
  new_columns <- dataview$columns_filter$selected #without keys
  
  # see https://stackoverflow.com/questions/24619628/passing-strings-as-arguments-in-dplyr-verbs
  filter_str <- rlang::parse_expr(get_keysfilter_str(dataview$keys_filter)) # e.g. rlang::parse_expr("PARAMCD == 'CRP' & AVISIT == 'BASELINE'")
  subset_data <- full_dataset$raw_data %>% 
      select(c(full_dataset$keys, new_columns)) %>%
      filter(!!filter_str) %>%
      select(c(new_keys, new_columns))
  Dataset(name=full_dataset$name, raw_data=subset_data, keys=new_keys) # we keep the name for merging -> detect identical columns
}
library(dplyr)
apply_dataview(ADLB_view)

is_named_list <- function(lst) {
  if (!is.list(lst)) {
    return(FALSE)
  }
  #browser()
  (is.null(names(lst)) && length(lst) == 0) || (!is.null(names(lst)) && sum(names(lst) != "") == length(lst))
}
#is_named_list(list())
#is_named_list(list(a=1))
#is_named_list(list(a=1, 2))

get_merged_column_names <- function(dataset_name, column_names) paste(dataset_name, column_names, sep=".")
merge_datasets <- function(datasets) {
  #stopifnot(is_named_list(datasets))
  # change column names for each dataset (prefix by dataname (not viewname!))
  datasets <- lapply(datasets, function(dataset) {
        data <- dataset$raw_data
        column_names <- setdiff(names(data), dataset$keys) # only rename non-keys
        names(column_names) <- get_merged_column_names(dataset$name, column_names)
        Dataset(name=dataset$name, raw_data = data %>% rename(!!!column_names), keys=dataset$keys)
      })
  stopifnot(all(vapply(datasets, function(dataset) identical(dataset$keys, datasets[[1]]$keys), TRUE)))
  keys <- datasets[[1]]$keys
  merged_data <- purrr::reduce(lapply(datasets, function(dataset) dataset$raw_data), full_join, by=keys)
  merged_dataname <- paste0("merged-", paste(vapply(datasets, function(dataset) dataset$name, ""), collapse="."))
  Dataset(name=merged_dataname, raw_data=merged_data, keys=keys)
}
# dataset_views <- list(ADLB_view, ADSL_view)
# dataset_subsets <- lapply(dataset_views, apply_dataview)
# merged_dataset <- merge_datasets(dataset_subsets)

library(ggplot2)
plot_view <- function(x_view, y_view) {
  dataset_views <- list(x_view, y_view)
  dataset_subsets <- lapply(dataset_views, apply_dataview)
  merged_dataset <- merge_datasets(dataset_subsets) 
  get_data_from_view <- function(view) merged_dataset$raw_data[get_merged_column_names(view$dataset$name, view$columns_filter$selected)]
  # only plot first column
  x_columns <- get_data_from_view(x_view)
  y_columns <- get_data_from_view(y_view)
  Map(function(x_name, x_vals) {
        Map(function(y_name, y_vals) {
              qplot(x_vals, y_vals) + xlab(x_name) + ylab(y_name)
            }, names(y_columns), y_columns)
      }, names(x_columns), x_columns)
  
}

# Shiny
ui_singleDatasetViewChooser <- function(id, singleDatasetViewSpec) {
  ns <- NS(id)
  
  #todo: ignoring show in keys_filter_spec and columns_filter_spec
  
  div(
      if (!is.null(singleDatasetViewSpec$keys_filter_spec)) {
            keys_filter_spec <- singleDatasetViewSpec$keys_filter_spec
            shiny::tagList(
                optionalSelectInput(
                    inputId = ns("keys_filter"),
                    label = "Keys Filter",
                    choices = keys_filter_spec$value_to_label(keys_filter_spec$choices),
                    selected = keys_filter_spec$value_to_label(keys_filter_spec$default_choice),
                    multiple = keys_filter_spec$multiple
                )
            )
          } else { 
            "No key filter"
          },
      if (singleDatasetViewSpec$columns_filter_spec$enabled) {
            columns_filter_spec <- singleDatasetViewSpec$columns_filter_spec
            optionalSelectInput(
                inputId = ns("columns_filter"),
                label = if (is.null(columns_filter_spec$label)) {
                      "Columns to select"
                    } else {
                      columns_filter_spec$label
                    },
                choices = columns_filter_spec$choices,
                selected = columns_filter_spec$default_choice,
                multiple = columns_filter_spec$multiple
            )
          } else {
            helpText("Selected columns:", tags$code(columns_filter_spec$default_choice))
          }
  )
}

srv_singleDatasetViewChooser <- function(input, output, session, datasets, singleDatasetViewSpec) {
  # datasets is provided by teal
  
  reactive({
        keys_filter <- if (!is.null(singleDatasetViewSpec$keys_filter_spec)) {
              list(
                  selected=singleDatasetViewSpec$keys_filter_spec$label_to_value(input$keys_filter),
                  keys=singleDatasetViewSpec$keys_filter_spec$keys
              )
            } else {
              NULL
            }
        columns_filter <- if (singleDatasetViewSpec$columns_filter_spec$enabled) {
              list(selected=input$columns_filter)
            } else {
              # column filter not enabled -> default choice
              list(selected=singleDatasetViewSpec$columns_filter_spec$default_choice)
            }
        DatasetView(view_name=singleDatasetViewSpec$view_name, dataset=singleDatasetViewSpec$dataset, keys_filter=keys_filter, columns_filter=columns_filter)
      })
}

id_for_dataset <- function(singleDatasetViewSpec) paste0("dataset-", singleDatasetViewSpec$view_name, "-details")

ui_DatasetViewsChooser <- function(id, datasetsViewSpec) {
  ns <- NS(id)
  
  #todo: don't show select if single dataset
  
  dataset_names <- vapply(datasetsViewSpec, function(singleDatasetViewSpec) singleDatasetViewSpec$dataset$name, "", USE.NAMES = FALSE)
  dataset_input <- optionalSelectInput(
      inputId = ns("dataset"),
      label = "Dataset",
      choices = dataset_names,
      selected = dataset_names[1],
      multiple = FALSE
  )
  conditional_singledatasetview_chooser <- function(singleDatasetViewSpec) {
    conditionalPanel(
        #todo: paste properly with quotes
        condition = paste0("input['", ns("dataset"), "'] == '", singleDatasetViewSpec$dataset$name, "'"),
        ui_singleDatasetViewChooser(ns(id_for_dataset(singleDatasetViewSpec)), singleDatasetViewSpec)
    )
  }
  div(dataset_input,
      do.call(div, unname(lapply(datasetsViewSpec, conditional_singledatasetview_chooser)))
  )
}

srv_DatasetViewsChooser <- function(input, output, session, datasets, datasetsViewSpec) {
  #todo: need to pass datasets to module??
  reactive({
        lapply(datasetsViewSpec, function(singleDatasetViewSpec) {
              callModule(srv_singleDatasetViewChooser, id_for_dataset(singleDatasetViewSpec), datasets=datasets, singleDatasetViewSpec=singleDatasetViewSpec)()
            })
      })
}

ui_DatasetViewsSummaryChooser <- function(id, datasetsViewSpec) {
  ns <- NS(id)
  
  standard_layout(
      output=plotOutput(ns("plots")),
      # tagList(
      #   uiOutput(ns("views_summary")),
      #   plotOutput(ns("plots"))
      # ),
      encoding=ui_DatasetViewsChooser(ns("views_chooser"), datasetsViewSpec)
  )
}

srv_DatasetViewsSummaryChooser <- function(input, output, session, datasets, datasetsViewSpec) {
  summarize_dataviews <- function(dataviews) {
    single_dataview <- function(singleDataview) {
      keys_list <- paste("Key combinations:", paste(singleDataview$keys_filter$selected, collapse=" OR "))
      columns_list <- paste("Selected columns:", paste(singleDataview$columns_filter, collapse=", "))
      tagList(
          h2(singleDataview$view_name),
          tags$ul(tags$li(keys_list), tags$li(columns_list))
      )
    }
    do.call(div,
        lapply(dataviews, single_dataview)
    )
    #paste(lapply(dataviews, single_dataview), collapse="####")
    #paste(dataviews, sep="----", collapse="****")
  }
  dataviews <- callModule(srv_DatasetViewsChooser, "views_chooser", datasets=datasets, datasetsViewSpec=datasetsViewSpec)
  output$plots <- renderPlot({
        #browser()
        plot_view(x_view=dataviews()[["x_view"]], y_view=dataviews()[["y_view"]])  
        # browser()
        # dataviews_evald <- isolate(dataviews())
        # plot_view(x_view=dataviews[["x_view"]], y_view=dataviews_evald[["y_view"]])  
        
      })
  output$views_summary <- renderUI({
        summarize_dataviews(dataviews())
      })
  return(dataviews)
}

SingleDataViewChooserModule <- function(
    label = "Single Data View Chooser",
    singleDatasetViewSpec=NULL,
    pre_output = NULL,
    post_output = NULL) {
  
  stopifnot(!is.null(singleDatasetViewSpec))
  
  args <- as.list(environment()) #why like this??
  
  teal::module(
      label = label,
      server = srv_singleDatasetViewChooser,
      ui = ui_singleDatasetViewChooser,
      ui_args = list(singleDatasetViewSpec),
      server_args = list(singleDatasetViewSpec),
      filters = NULL
  )
}

DataViewsChooserModule <- function(
    label = "Single Data View Chooser",
    datasetViewsSpec=NULL,
    pre_output = NULL,
    post_output = NULL) {
  
  stopifnot(!is.null(datasetViewsSpec))
  
  args <- as.list(environment()) #why like this??
  
  teal::module(
      label = label,
      server = srv_DatasetViewsChooser,
      ui = ui_DatasetViewsChooser,
      ui_args = list(datasetViewsSpec),
      server_args = list(datasetViewsSpec),
      filters = NULL
  )
}

DataViewsChooserSummaryModule <- function(
    label = "Single Data View Chooser",
    datasetViewsSpec=NULL,
    pre_output = NULL,
    post_output = NULL) {
  
  stopifnot(!is.null(datasetViewsSpec))
  
  args <- as.list(environment()) #why like this??
  
  teal::module(
      label = label,
      server = srv_DatasetViewsSummaryChooser,
      ui = ui_DatasetViewsSummaryChooser,
      ui_args = list(datasetViewsSpec),
      server_args = list(datasetViewsSpec),
      filters = NULL
  )
}


CDISC_Data <- function(...) {
  list(...)
}

library(teal)
x <- teal::init(
    data = CDISC_Data(
        ASL = ADSL_raw, #todo: adapt so that keys are kept, not used currently
        ADLB_raw = ADLB_raw
    ),
    modules = root_modules(
        # SingleDataViewChooserModule(
        #   singleDatasetViewSpec=ADLB_viewspec
        # )
        # SingleDataViewChooserModule(
        #   singleDatasetViewSpec=ADSL_viewspec
        # )
        # DataViewsChooserModule(
        #   datasetViewsSpec=list(ADLB_viewspec, ADSL_viewspec)
        # )
        DataViewsChooserSummaryModule(
            datasetViewsSpec=list(x_view=ADLB_viewspec, y_view=ADSL_viewspec)
        )
    )
)
shinyApp(x$ui, x$server)


# ?standard_layout
# 
# library(shiny)
# 
# plot_xy <- function(x, y) {
#   datasetViews <- DatasetsViewChooser(datasets=list(x, y))
#   
#   check_identical_keys()
#   # merge them, then pick them
#   
# }
# 
# plot_xy(x=ADSL_view, y=ADLB_view)
# 
# 
# 
# 






