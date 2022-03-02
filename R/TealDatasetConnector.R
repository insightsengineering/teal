## TealDatasetConnector ====
#'
#'
#' @title A `TealDatasetConnector` class of objects
#'
#' @description `r lifecycle::badge("stable")`
#' Objects of this class store the connection function to fetch a single dataset.
#' Note that for some specific connection types,
#' an object of class `TealDataConnection` must be provided.
#' Data can be pulled via the `pull` method and accessed directly
#' through the `dataset` active binding.
#' Pulled data inherits from the class [`TealDataset`]
#'
#' @param dataname (`character`)\cr
#'  A given name for the dataset it may not contain spaces
#'
#' @param pull_callable (`CallableFunction`)\cr
#'   function with necessary arguments set to fetch data from connection.
#'
#' @param keys optional, (`character`)\cr
#'  vector of dataset primary keys column names
#'
#' @param label (`character`)\cr
#'  Label to describe the dataset.
#'
#' @param code (`character`)\cr
#'  A character string defining code to modify `raw_data` from this dataset. To modify
#'  current dataset code should contain at least one assignment to object defined in `dataname`
#'  argument. For example if `dataname = ADSL` example code should contain
#'  `ADSL <- <some R code>`. Can't be used simultaneously with `script`
#'
#' @param script (`character`)\cr
#'   Alternatively to `code` - location of the file containing modification code.
#'   Can't be used simultaneously with `script`.
#'
#' @param vars (named `list`)) \cr
#'   In case when this object code depends on other `TealDataset` object(s) or
#'   other constant value, this/these object(s) should be included as named
#'   element(s) of the list. For example if this object code needs `ADSL`
#'   object we should specify `vars = list(ADSL = <adsl object>)`.
#'   It's recommended to include `TealDataset` or `TealDatasetConnector` objects to
#'   the `vars` list to preserve reproducibility. Please note that `vars`
#'   are included to this object as local `vars` and they cannot be modified
#'   within another dataset.
#'
#' @param metadata (named `list`, `NULL` or `CallableFunction`) \cr
#'   Field containing either the metadata about the dataset (each element of the list
#'   should be atomic and length one) or a `CallableFuntion` to pull the metadata
#'   from a connection. This should return a `list` or an object which can be
#'   converted to a list with `as.list`.
TealDatasetConnector <- R6::R6Class( # nolint

  ## __Public Methods ====
  classname = "TealDatasetConnector",
  public = list(
    #' @description
    #' Create a new `TealDatasetConnector` object. Set the pulling function
    #' `CallableFunction` which returns a `data.frame` or `MultiAssayExperiment`,
    #' e.g. by reading from a function or creating it on the fly.
    initialize = function(dataname,
                          pull_callable,
                          keys = character(0),
                          label = character(0),
                          code = character(0),
                          vars = list(),
                          metadata = NULL) {
      private$set_pull_callable(pull_callable)
      private$set_var_r6(vars)
      private$set_pull_vars(vars)

      private$set_dataname(dataname)
      private$set_metadata(metadata)

      self$set_dataset_label(label)
      self$set_keys(keys)

      if (length(code) > 0) {
        # just needs a dummy TealDataset object to store mutate code, hence col = 1
        private$dataset <- TealDataset$new(dataname = self$get_dataname(), x = data.frame(col = 1))
        private$dataset$mutate(code = code, vars = vars, force_delay = TRUE)
      }

      logger::log_trace("TealDatasetConnector initialized for dataset: { deparse1(self$get_dataname()) }.")

      return(invisible(self))
    },
    #' @description
    #' Prints this `TealDatasetConnector`.
    #'
    #' @param ... additional arguments to the printing method
    #' @return invisibly self
    print = function(...) {
      check_ellipsis(...)

      cat(sprintf(
        "A %s object, named %s, containing a TealDataset object that has %sbeen loaded/pulled%s\n",
        class(self)[1],
        self$get_dataname(),
        ifelse(self$is_pulled(), "", "not "),
        ifelse(self$is_pulled(), ":", "")
      ))
      if (self$is_pulled()) {
        print(self$get_dataset())
      }

      invisible(self)
    },

    # ___ getters ====
    #' @description
    #' Get dataname of dataset
    #'
    #' @return dataname of the dataset
    get_dataname = function() {
      return(private$dataname)
    },
    #' @description
    #' Get dataname of dataset
    #'
    #' @return `character` dataname of the dataset
    get_datanames = function() {
      return(private$dataname)
    },
    #' @description
    #' Get label of dataset
    #'
    #' @return `character` dataset label
    get_dataset_label = function() {
      return(private$dataset_label)
    },
    #' @description
    #' Get primary keys of dataset
    #' @return `character` vector with dataset primary keys
    get_keys = function() {
      return(private$keys)
    },
    #' @description
    #' Get `JoinKeys` object with keys used for joining.
    #' @return (`JoinKeys`)
    get_join_keys = function() {
      if (is.null(private$join_keys)) {
        private$join_keys <- join_keys()
      }
      private$join_keys
    },

    #' @description
    #' Get code to get data
    #'
    #' @param deparse (`logical`)\cr
    #'  whether return deparsed form of a call
    #'
    #' @return optionally deparsed `call` object
    get_code = function(deparse = TRUE) {
      checkmate::assert_flag(deparse)
      return(self$get_code_class()$get_code(deparse = deparse))
    },
    #' @description
    #' Get internal `CodeClass` object
    #'
    #' @return `CodeClass`
    get_code_class = function() {
      code_class <- CodeClass$new()
      pull_code_class <- private$get_pull_code_class()
      code_class$append(pull_code_class)

      if (!is.null(private$dataset)) {
        executed_code_in_dataset <- private$dataset$get_code_class()
        code_class$append(executed_code_in_dataset)
      }

      return(code_class)
    },
    #' @description
    #'
    #' Derive the arguments this connector will pull with
    #' @return `list` of pull function fixed arguments
    get_pull_args = function() {
      private$pull_callable$get_args()
    },
    #' @description
    #' Get dataset
    #'
    #' @return dataset (`TealDataset`)
    get_dataset = function() {
      if (!self$is_pulled()) {
        stop(
          sprintf("'%s' has not been pulled yet\n - please use `load_dataset()` first.", self$get_dataname()),
          call. = FALSE
        )
      }
      private$dataset$merge_join_keys(self$get_join_keys())
      private$dataset$get_dataset()
      return(private$dataset)
    },
    #' @description
    #' Get error message from last pull
    #'
    #' @return `character` object with error message or `character(0)` if last
    #'  pull was successful.
    get_error_message = function() {
      return(private$pull_callable$get_error_message())
    },
    #' @description
    #' Get pull function
    #'
    #' @return `CallableFunction`
    get_pull_callable = function() {
      return(private$pull_callable)
    },
    #' @description
    #' Get raw data from dataset
    #'
    #' @return `data.frame` or `MultiAssayExperiment` data
    get_raw_data = function() {
      dataset <- self$get_dataset()
      return(dataset$get_raw_data())
    },
    #' @description
    #' Get the list of dependencies that are `TealDataset` or `TealDatasetConnector` objects
    #'
    #' @return `list`
    get_var_r6 = function() {
      return(private$var_r6)
    },

    # ___ setters ====
    #' @description
    #' Reassign `vars` in this object to keep references up to date after deep clone.
    #' Update is done based on the objects passed in `datasets` argument. Reassignment
    #' refers only to the provided `datasets`, other `vars` remains the same.
    #' @param datasets (`named list` of `TealDataset(s)` or `TealDatasetConnector(s)`)\cr
    #'   objects with valid pointers.
    #' @return NULL invisible
    reassign_datasets_vars = function(datasets) {
      logger::log_trace(
        "TealDatasetConnector$reassign_datasets_vars reassigning vars in dataset: { self$get_dataname() }."
      )
      checkmate::assert_list(datasets, min.len = 0, names = "unique")

      common_var_r6 <- intersect(names(datasets), names(private$var_r6))
      private$var_r6[common_var_r6] <- datasets[common_var_r6]

      common_vars <- intersect(names(datasets), names(private$pull_vars))
      private$pull_vars[common_vars] <- datasets[common_vars]

      if (!is.null(private$dataset)) {
        private$dataset$reassign_datasets_vars(datasets)
      }
      logger::log_trace(
        "TealDatasetConnector$reassign_datasets_vars reassigned vars in dataset: { self$get_dataname() }."
      )

      invisible(NULL)
    },
    #' @description
    #' Set label of the `dataset` object
    #'
    #' @return (`self`) invisibly for chaining
    set_dataset_label = function(label) {
      if (is.null(label)) {
        label <- character(0)
      }
      checkmate::assert_character(label, max.len = 1, any.missing = FALSE)
      private$dataset_label <- label
      if (self$is_pulled()) {
        private$dataset$set_dataset_label(label)
      }
      logger::log_trace(
        "TealDatasetConnector$set_dataset_label label set for dataset: { deparse1(self$get_dataname()) }."
      )

      return(invisible(self))
    },
    #' @description
    #' Set new keys
    #' @return (`self`) invisibly for chaining.
    set_keys = function(keys) {
      checkmate::assert_character(keys, any.missing = FALSE)
      if (isTRUE(self$is_pulled())) {
        set_keys(private$dataset, keys)
      }
      private$keys <- keys
      logger::log_trace("TealDatasetConnector$set_keys keys set for dataset: { deparse1(self$get_dataname()) }.")

      return(invisible(self))
    },
    #' @description
    #' set join_keys for a given dataset and self
    #' @param x `list` of `JoinKeySet` objects (which are created using the `join_key` function)
    #' or single `JoinKeySet` objects
    #' @return (`self`) invisibly for chaining
    set_join_keys = function(x) {
      self$get_join_keys()$set(x)
      logger::log_trace(paste(
        "TealDatasetConnector$set_join_keys join_keys set for dataset:",
        "{ deparse1(self$get_dataname()) }."
      ))

      return(invisible(self))
    },

    #' @description
    #' mutate the join_keys for a given dataset and self
    #' @param dataset (`character`) dataset for which join_keys are to be set against self
    #' @param val (named `character`) column names used to join
    #' @return (`self`) invisibly for chaining
    mutate_join_keys = function(dataset, val) {
      self$get_join_keys()$mutate(private$dataname, dataset, val)
      logger::log_trace(
        "TealDatasetConnector$mutate_join_keys join_keys modified keys of
        { deparse1(self$get_dataname()) } against { dataset }."
      )

      return(invisible(self))
    },

    # ___ pull ====
    #' @description
    #' Pull the data (and metadata if it is a `Callable`)
    #'
    #' Read or create data using `pull_callable` specified in the constructor.
    #'
    #' @param args (`NULL` or named `list`)\cr
    #'  additional dynamic arguments for pull function. `args` can be omitted if `pull_callable`
    #'  from constructor already contains all necessary arguments to pull data. One can try
    #'  to execute `pull_callable` directly by `x$pull_callable$run()` or to get code using
    #'  `x$pull_callable$get_code()`. `args` specified in pull are used temporary to get data but
    #'  not saved in code.
    #' @param try (`logical` value)\cr
    #'  whether perform function evaluation inside `try` clause
    #'
    #' @return (`self`) if successful.
    pull = function(args = NULL, try = FALSE) {
      logger::log_trace("TealDatasetConnector$pull pulling dataset: {self$get_dataname() }.")
      data <- private$pull_internal(args = args, try = try)
      if (!self$is_failed()) {
        # The first time object is pulled, private$dataset may be NULL if mutate method was never called
        has_dataset <- !is.null(private$dataset)
        if (has_dataset) {
          code_in_dataset <- private$dataset$get_code_class(nodeps = TRUE)
          vars_in_dataset <- private$dataset$get_vars()
        }

        pulled_metadata <- private$pull_metadata_internal()
        private$dataset <- dataset(
          dataname = self$get_dataname(),
          x = data,
          keys = character(0), # keys need to be set after mutate
          label = self$get_dataset_label(),
          code = private$get_pull_code_class(),
          metadata = pulled_metadata
        )

        if (has_dataset) {
          private$dataset$mutate(
            code = code_in_dataset,
            vars = vars_in_dataset
          )
        }
        set_keys(private$dataset, self$get_keys())
        private$is_pulled_flag <- TRUE
        logger::log_trace("TealDatasetConnector$pull pulled dataset: {self$get_dataname() }.")
      } else {
        logger::log_error("TealDatasetConnector$pull failed to pull dataset: {self$get_dataname() }.")
      }

      return(invisible(self))
    },
    #' @description
    #' Set arguments to the pulling function
    #'
    #' @param args (`NULL` or named `list`) dynamic arguments to function
    #'
    #' @return (`self`) invisibly for chaining
    set_args = function(args) {
      set_args(private$pull_callable, args)
      logger::log_trace("TealDatasetConnector$set_args pull args set for dataset: {self$get_dataname() }.")
      return(invisible(self))
    },

    # ___ mutate ====
    #' @description
    #' Dispatcher for either eager or delayed mutate methods
    #'
    #' Either code or script must be provided, but not both.
    #'
    #' @return (`self`) invisibly for chaining.
    mutate = function(code, vars = list()) {
      checkmate::assert_list(vars, min.len = 0, names = "unique")

      if (is.null(private$dataset)) {
        # just needs a dummy TealDataset object to store mutate code, hence col = 1
        private$dataset <- TealDataset$new(dataname = self$get_dataname(), x = data.frame(col = 1))
      }
      private$dataset$mutate(code = code, vars = vars, force_delay = !self$is_pulled())
      # should be called at the end so that failure in TealDataset object will prevent it.
      private$set_var_r6(vars)
      logger::log_trace(
        sprintf(
          "TealDatasetConnector$mutate mutated dataset '%s' using the code (%s lines) and vars (%s).",
          self$get_dataname(),
          length(parse(text = if (is(code, "CodeClass")) code$get_code() else code)),
          paste(names(vars), collapse = ", ")
        )
      )


      return(invisible(self))
    },

    # ___ status ====
    #' @description
    #' Check if pull has not failed.
    #'
    #' @return `TRUE` if pull failed, else `FALSE`
    is_failed = function() {
      return(private$pull_callable$is_failed())
    },
    #' @description
    #' Check if dataset has already been pulled.
    #'
    #' @return `TRUE` if connector has been already pulled, else `FALSE`
    is_pulled = function() {
      private$is_pulled_flag
    },
    #' @description
    #' Check if dataset has mutations that are delayed
    #'
    #' @return `logical`
    is_mutate_delayed = function() {
      if (is.null(private$dataset)) {
        FALSE
      } else {
        private$dataset$is_mutate_delayed()
      }
    },

    # ___ check ====
    #' @description
    #' Check to determine if the raw data is reproducible from the
    #' `get_code()` code.
    #' @return
    #' `TRUE` always for all connectors to avoid evaluating the same code multiple times.
    check = function() {
      return(TRUE)
    },
    # ___ shiny ====
    #' @description
    #' Sets the shiny UI according to the given inputs.
    #' Inputs must provide only scalar (length of 1) variables.
    #' @param inputs (`function`) A shiny module UI function with single argument `ns`.
    #' This function needs to return a list of shiny inputs with their `inputId` wrapped
    #' in function `ns`. The `inputId` must match exactly the argument name to be set.
    #' See example.
    #' Nested lists are not allowed.
    #' @return (`self`) invisibly for chaining.
    #' @examples
    #' ds <- dataset_connector("xyz", pull_callable = callable_function(data.frame))
    #' ds$set_ui_input(
    #'   function(ns) {
    #'     list(sliderInput(ns("colA"), "Select value for colA", min = 0, max = 10, value = 3),
    #'          sliderInput(ns("colB"), "Select value for colB", min = 0, max = 10, value = 7))
    #'   }
    #' )
    #' \dontrun{
    #' ds$launch()
    #' }
    set_ui_input = function(inputs = NULL) {
      stopifnot(is.null(inputs) || is.function(inputs))
      if (is.function(inputs)) {
        if (!identical(names(formals(inputs)), "ns")) {
          stop("'inputs' must be a function of a single argument called 'ns'")
        }
      }
      private$ui_input <- inputs
      logger::log_trace(
        "TealDatasetConnector$set_ui_input ui_input set for dataset: { deparse1(self$get_dataname()) }."
      )
      return(invisible(self))
    },
    #' @description
    #' Get shiny ui function
    #' @param id (`character`) namespace id
    #' @return shiny UI in given namespace id
    get_ui = function(id) {
      checkmate::assert_string(id)
      if (!is.null(private$ui)) {
        private$ui(id)
      }
    },
    #' @description
    #' Get shiny server function
    #' @return shiny server function
    get_server = function() {
      return(private$server)
    },
    #' @description
    #' Launches a shiny app.
    #' @return Shiny app
    #' @examples
    #' ds <- dataset_connector("xyz", pull_callable = callable_function(data.frame))
    #' ds$set_ui_input(
    #'   function(ns) {
    #'     list(sliderInput(ns("colA"), "Select value for colA", min = 0, max = 10, value = 3),
    #'          sliderInput(ns("colB"), "Select value for colB", min = 0, max = 10, value = 7))
    #'   }
    #' )
    #' \dontrun{
    #' ds$launch()
    #' }
    launch = function() {
      if (is.null(private$server)) {
        stop("No arguments set yet. Please use set_ui_input method first.")
      }
      shinyApp(
        ui = fluidPage(
          self$get_ui(id = "main_app"),
          shinyjs::useShinyjs(),
          br(),
          actionButton("pull", "Get data"),
          br(),
          tableOutput("result")
        ),
        server = function(input, output, session) {
          session$onSessionEnded(stopApp)
          observeEvent(input$pull, {
            self$get_server()(id = "main_app")
            if (self$is_pulled()) {
              output$result <- renderTable(head(self$get_raw_data()))
            }
          })
        }
      )
    }
  ),
  ## __Private Fields ====
  private = list(
    dataset = NULL, # TealDataset
    pull_callable = NULL, # Callable
    pull_vars = list(), # named list
    dataname = character(0),
    dataset_label = character(0),
    metadata = NULL, # Callable or list
    keys = NULL,
    var_r6 = list(),
    ui_input = NULL, # NULL or list
    is_pulled_flag = FALSE,
    join_keys = NULL,

    ## __Private Methods ====
    ui = function(id) {
      ns <- NS(id)
      # add namespace to input ids
      ui <- if (!is.null(private$ui_input)) {
        do.call(private$ui_input, list(ns = ns))
      } else {
        NULL
      }
      # check ui inputs
      if (!is.null(ui)) {
        checkmate::assert_list(ui, types = "shiny.tag")
        attr_class <- vapply(lapply(ui, "[[", i = "attribs"), "[[", character(1), i = "class")
        if (!all(grepl("shiny-input-container", attr_class))) {
          stop("All elements must be shiny inputs")
        }
      }
      # create ui
      if (!is.null(ui)) {
        tags$div(
          tags$div(
            id = ns("inputs"),
            h4("TealDataset Connector for ", code(self$get_dataname())),
            ui
          )
        )
      }
    },
    server = function(id, data_args = NULL) {
      moduleServer(
        id = id,
        function(input, output, session) {
          withProgress(value = 1, message = paste("Pulling", self$get_dataname()), {
            # set args to save them - args set will be returned in the call
            dataset_args <- if (!is.null(private$ui_input)) {
              reactiveValuesToList(input)
            } else {
              NULL
            }
            if (length(dataset_args) > 0) {
              self$set_args(args = dataset_args)
            }

            self$pull(args = data_args, try = TRUE)

            # print error if any
            # error doesn't break an app
            if (self$is_failed()) {
              shinyjs::alert(
                sprintf(
                  "Error pulling %s:\nError message: %s",
                  self$get_dataname(),
                  self$get_error_message()
                )
              )
            }
          })
        }
      )
      return(invisible(self))
    },

    # need to have a custom deep_clone because one of the key fields are reference-type object
    # in particular: dataset is a R6 object that wouldn't be cloned using default clone(deep = T)
    deep_clone = function(name, value) {
      deep_clone_r6(name, value)
    },
    get_pull_code_class = function(args = NULL) {
      res <- CodeClass$new()
      res$append(list_to_code_class(private$pull_vars))
      code <- if (inherits(private$pull_callable, "CallableCode")) {
        tmp <- private$pull_callable$get_call(deparse = FALSE)
        tmp[[length(tmp)]] <- substitute(a <- b, list(a = as.name(private$dataname), b = tmp[[length(tmp)]]))
        paste0(vapply(tmp, deparse1, character(1), collapse = "\n"), collapse = "\n")
      } else {
        deparse1(substitute(
          a <- b,
          list(
            a = as.name(private$dataname),
            b = private$pull_callable$get_call(deparse = FALSE, args = args)
          )
        ), collapse = "\n")
      }

      res$set_code(code = code, dataname = private$dataname, deps = names(private$pull_vars))
      return(res)
    },
    set_pull_callable = function(pull_callable) {
      stopifnot(is(pull_callable, "Callable"))
      private$pull_callable <- pull_callable
      return(invisible(self))
    },
    set_metadata = function(metadata) {
      if (methods::is(metadata, "Callable")) {
        private$metadata <- metadata
      } else {
        validate_metadata(metadata)
        private$metadata <- metadata
      }
      return(invisible(self))
    },
    set_pull_vars = function(pull_vars) {
      checkmate::assert_list(pull_vars, min.len = 0, names = "unique")
      private$pull_vars <- pull_vars
      return(invisible(self))
    },
    pull_metadata_internal = function() {
      if (!checkmate::test_class(private$metadata, "Callable")) {
        return(private$metadata)
      }

      logger::log_trace("TealDatasetConnector$pull pulling metadata for dataset: {self$get_dataname() }.")
      pulled_metadata <- private$metadata$run(try = TRUE)

      if (checkmate::test_class(pulled_metadata, c("simpleError", "error"))) {
        logger::log_warn("TealDatasetConnector$pull pulling metadata failed for dataset: {self$get_dataname() }.")
        return(NULL)
      }

      # metadata pulled, now lets make sure it is valid
      tryCatch(
        {
          pulled_metadata <- as.list(pulled_metadata)
          validate_metadata(pulled_metadata)
          logger::log_trace("TealDatasetConnector$pull pulled metadata for dataset: {self$get_dataname() }.")
          return(pulled_metadata)
        },
        error = function(e) {
          logger::log_warn("TealDatasetConnector$pull invalid metadata for dataset: {self$get_dataname() }.")
          return(NULL)
        }
      )
    },
    pull_internal = function(args = NULL, try = FALSE) {
      # include objects CallableFunction environment
      if (length(private$pull_vars) > 0) {
        for (var_idx in seq_along(private$pull_vars)) {
          var_name <- names(private$pull_vars)[[var_idx]]
          var_value <- private$pull_vars[[var_idx]]

          # assignment is done in pull_callable only once
          # because x is locked within local environment
          # this means that re-assignment is not possible and will be silently skipped
          # During the app loading, assign is called only once.
          private$pull_callable$assign_to_env(
            x = var_name,
            value = if (is(var_value, "TealDatasetConnector") || is(var_value, "TealDataset")) {
              get_raw_data(var_value)
            } else {
              var_value
            }
          )
        }
      }
      # eval CallableFunction with dynamic args
      tryCatch(
        expr = private$pull_callable$run(args = args, try = try),
        error = function(e) {
          if (grepl("object 'conn' not found", e$message)) {
            output_message <- "This dataset connector requires connection object (conn) to be provided."
          } else {
            output_message <- paste("Could not pull dataset, the following error message was returned:", e$message)
          }
          stop(output_message, call. = FALSE)
        }
      )
    },
    set_failure = function(res) {
      if (is(res, "error")) {
        private$failed <- TRUE
        private$failure_msg <- conditionMessage(res)
      } else {
        private$failed <- FALSE
        private$failure_msg <- NULL
      }
      return(NULL)
    },
    set_var_r6 = function(vars) {
      checkmate::assert_list(vars, min.len = 0, names = "unique")
      for (varname in names(vars)) {
        var <- vars[[varname]]

        if (is(var, "TealDatasetConnector") || is(var, "TealDataset")) {
          var_deps <- var$get_var_r6()
          var_deps[[varname]] <- var
          for (var_dep_name in names(var_deps)) {
            var_dep <- var_deps[[var_dep_name]]
            if (identical(self, var_dep)) {
              stop("Circular dependencies detected")
            }
            private$var_r6[[var_dep_name]] <- var_dep
          }
        }
      }
      return(invisible(self))
    },
    set_dataname = function(dataname) {
      checkmate::assert_string(dataname)
      stopifnot(!grepl("\\s", dataname))
      private$dataname <- dataname
      return(invisible(self))
    },
    set_ui = function(ui_args = NULL) {
      private$ui <- function(id) {
        ns <- NS(id)
        # add namespace to input ids
        ui <- if (!is.null(ui_args)) {
          do.call(ui_args, list(ns = ns))
        } else {
          NULL
        }
        # check ui inputs
        if (!is.null(ui)) {
          checkmate::assert_list(ui, types = "shiny.tag")
          attr_class <- vapply(lapply(ui, "[[", i = "attribs"), "[[", character(1), i = "class")
          if (!all(grepl("shiny-input-container", attr_class))) {
            stop("All elements must be shiny inputs")
          }
        }
        # create ui
        if (!is.null(ui)) {
          tags$div(
            tags$div(
              id = ns("inputs"),
              h4("TealDataset Connector for ", code(self$get_dataname())),
              ui
            )
          )
        }
      }
      return(invisible(self))
    }
  )
)
