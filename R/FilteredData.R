#' @title Class to encapsulate filtered data sets
#'
#' @details This class encapsulates data import and filtering.
#'   Once a dataname was specified it won't be deleted at runtime to keep
#'   reactive relations intact. If a dataset is not needed anymore then you can
#'   set it to NULL with load_data.
#'   Every data set is also filtered with ASL, hence ASL is given by default (NULL)
#'
#' load_data
#' 1 load data
#' 2 specify filer_info
#' 3 apply filters
#'
#' @examples
#' \dontrun{
#' path <- "/opt/BIOSTAT/qa/cdt7876a/libraries/asl.sas7bdat"
#'
#' x <- FilteredData$new()
#'
#' x$load_data(path)
#'
#' x$datanames()
#'
#' x$list_data_info("ASL")
#' x$get_filter_info("ASL")
#'
#' df <- x$get_data("ASL")
#'
#' x$get_filter_info("ASL")[['USUBJID']]$type
#'
#' x$set_filter("ASL", list(AGE=c(3,5), SEX=c('M', 'F')))
#' }
FilteredData <- R6::R6Class( # nolint
  "FilteredData",
  ## FilteredData ====
  ## __Public Methods ====
  public = list(

    initialize = function(datanames = c("ASL")) {

      for (dataname in datanames) {
        if (grepl("[[:space:]]", dataname))
          stop(paste0("invalid dataname '", dataname, "' datanames without spaces"))
      }

      private$init_datanames <- datanames

      create_rv <- function()  do.call(reactiveValues, setNames(lapply(datanames, function(x)NULL), datanames))

      private$datasets          <- create_rv()
      private$filtered_datasets <- create_rv()
      private$filter_state      <- create_rv()

      invisible(self)
    },


    datanames = function() {
      dn <- isolate(.subset2(private$datasets, "impl")$names())

      c(intersect(private$init_datanames, dn), setdiff(dn, private$init_datanames))
    },




    load_data = function(path, dataname=NULL, ...) {

      if (is.null(path) || !file.exists(path)) stop(paste("invalid path:", path))

      logger_in()
      .log("load data:", path)

      dataname <- if (is.null(dataname)) {
        tools::file_path_sans_ext(basename(path))
      } else {
        dataname
      }

      # currently only allow datanames that were defined in initialize
      private$error_if_not_valid(dataname)

      path <- normalizePath(path, mustWork = TRUE)

      df <- switch(tolower(tools::file_ext(path)),
                   sas7bdat = haven::read_sas(path, ...),
                   csv      = readr::read_csv(path, ...),
                   rds      = readRDS(path, ...),
                   stop(paste("The format of", path, "is currently not supported."))
      )

      attr(df, "path") <- path
      attr(df, "md5sum") <- tools::md5sum(path)
      attr(df, "last_modified") <- file.info(path)$mtime[1]

      .log("load data", dataname)
      logger_out()

      self$set_data(dataname, df)

    },

    set_data = function(dataname, data) {

      private$datasets[[dataname]] <- data
      private$update_filter_info(dataname)

      private$apply_filter(dataname)

      invisible(self)
    },


    list_data_info = function(dataname, filtered=FALSE, variables=NULL) {

      log2 <- function(...) {
        cat(paste(..., collapse = " ")); cat("\n")
      }

      private$error_if_not_valid(dataname)

      log2("====", dataname, "=======================")

      df <- isolate(private$datasets[[dataname]])
      if (is.null(df)) {
        log2("The", dataname, "data is NULL")
      } else {

        fi <- private$filter_info[[dataname]]

        vars <- if (filtered) {

          x <- names(self$get_filter_state(dataname))

          if (is.null(x)) character(0) else x

        } else {
          names(df)
        }

        if (!is.null(variables)) {
          vars <- intersect(vars, variables)
        }

        nmax <- if (length(vars) > 0) max(nchar(vars)) else 0

        for (name in vars) {

          fi_i <- fi[[name]]

          info <- switch(
            fi_i$type,
            range = paste(fi_i$range, collapse = " - "),
            choices = {
              if (length(fi_i$choices) > 5) {
                paste0(paste(fi_i$choices[1:5], collapse = ", "), ", ...")
              } else {
                paste(fi_i$choices, collapse = ", ")
              }
            },
            logical = paste(fi_i$choices, collapse = ", "),
            ""
          )

          log2(sprintf(paste0("%-", nmax, "s has filter type %-10s: %s"), name, fi_i$type, info))

        }
      }
      log2("===========================")
    },




    reset_data = function(dataname = NULL) {

      datanames <- if (is.null(dataname)) {
        self$datanames() # delete all datasets
      } else {
        private$error_if_not_valid(dataname)
        dataname
      }

      for (name in datanames) {
        private$datasets[[name]]                <- NULL
        private$filtered_datasets[[name]]       <- NULL
        private$filter_state[[name]]            <- NULL
        private$filter_info[[name]]             <- NULL
      }

      # run if only asl data was reset
      if (identical(dataname, "ASL")) private$apply_filter()

      invisible(self)
    },




    # dataname is valid and data is not null
    has_data = function(dataname) {
      dataname %in% self$datanames() && !is.null(self$get_data(dataname))
    },



    has_variable = function(dataname, varname) {
      self$has_data(dataname) && (varname %in% names(self$get_data(dataname)))
    },



    get_data = function(dataname, reactive=FALSE, filtered=FALSE) {

      private$error_if_not_valid(dataname)

      f <- if (reactive) {
        function(x) x
      } else {
        function(x) isolate(x)
      }

      if (filtered) {
        f(private$filtered_datasets[[dataname]])
      } else {
        f(private$datasets[[dataname]])
      }
    },



    get_filter_info = function(dataname, varname=NULL) {
      private$error_if_not_valid(dataname, varname)
      if (is.null(varname)) {
        private$filter_info[[dataname]]
      } else {
        private$filter_info[[dataname]][[varname]]
      }
    },

    # TODO add remove_filter method

    get_filter_type = function(dataname, varname) {
      private$error_if_not_valid(dataname, varname)
      private$filter_info[[dataname]][[varname]][["type"]]
    },

    set_filter_state = function(dataname, varname=NULL, state) {

      # varname = NULL > for all variables
      # state = NULL erase filter state
      #
      # varname != NULL and state is missing, set to default value

      private$error_if_not_valid(dataname, varname)

      if (is.null("state")) stop("use remove_filter and not set_filter_state if to remove a filter")

      if (is.null(varname)) {
        if (!(is.null(state) || is.list(state)))
          stop("filter state needs to be a list when specified for all variables")

        fs_names <- names(state)
        varnames <- names(self$get_data(dataname))

        # check if variables exist
        non_valid_vars_i <- which(!(fs_names %in% varnames))
        if (length(non_valid_vars_i) > 0) {
          stop(paste("variables", paste(fs_names[non_valid_vars_i], collapse = ", "),
                     "are not available in data", dataname))
        }

        # check if filter state is possible
        fi <- private$filter_info[[dataname]]

        for (var in fs_names) {
          fii <- fi[[var]]
          state_i <- state[[var]]

          switch(
            fii$type,
            choices = {
              if (any(!(state_i %in% fii$choices)))
                stop(paste("data", dataname, "variable", var, "not all valid choices"))
            },
            range = {
              if (length(state_i) != 2)
                stop(paste("data", dataname, "variable", var, "not of length 2"))
            },
            logical = NULL,
            stop(paste("data", dataname, "variable", var, ": cannot filter this variable (type issue)"))
          )
        }

        private$filter_state[[dataname]] <- state

      } else {
        ## TODO: copy paste from above, change eventually

        fii <- self$get_filter_info(dataname, varname)
        state_i <- state; var <- varname
        switch(
          fii$type,
          choices = {
            if (length(state_i) > 0 && any(!(state_i %in% fii$choices)))
              stop(paste("data", dataname, "variable", var, "not all valid choices"))
          },
          range = {
            if (length(state_i) != 2)
              stop(paste("data", dataname, "variable", var, "not of length 2"))
          },
          logical = NULL,
          stop(paste("data", dataname, "variable", var, ": cannot filter this variable (type issue)"))
        )

        fs <- self$get_filter_state(dataname)
        if (is.null(fs)) fs <- list()
        fs[[varname]] <- state
        private$filter_state[[dataname]] <- fs
      }

      private$apply_filter(dataname)

      invisible(self)
    },



    remove_filter = function(dataname, varname) {

      private$error_if_not_valid(dataname, varname)

      fs <- self$get_filter_state(dataname)
      fs[[varname]] <- NULL
      private$filter_state[[dataname]] <- fs

      private$apply_filter(dataname)

      invisible(self)
    },



    get_filter_state = function(dataname, varname=NULL, reactive=FALSE) {
      private$error_if_not_valid(dataname, varname)

      if (is.null(varname)) {
        if (reactive) {
          private$filter_state[[dataname]]
        } else {
          isolate(private$filter_state[[dataname]])
        }
      } else {
        if (reactive) stop("can not access reactive value for indiviual variable")

        isolate(private$filter_state[[dataname]][[varname]])
      }
    },




    set_default_filter_state = function(dataname, varname, nchoices = 10) {
      private$error_if_not_valid(dataname, varname)

      fi <- self$get_filter_info(dataname, varname)

      state <- switch(
        fi$type,
        choices = {
          if (length(fi$choices) > nchoices) {
            character(0)
          } else {
            fi$choices
          }
        },
        range = fi$range,
        logical = "TRUE or FALSE",
        stop("unknown type")
      )

      self$set_filter_state(dataname, varname, state)

      invisible(self)
    },



    is_filter_variable = function(dataname, varname) {
      self$get_filter_type(dataname, varname) != "unknown"
    },



    get_filter_call = function(dataname, merge=TRUE, asl=TRUE) {

      private$error_if_not_valid(dataname)

      asl_filter_call <- private$get_subset_call("ASL", "ASL_FILTERED")

      if (dataname == "ASL") {
        asl_filter_call
      } else {

        out <- paste0(dataname, "_FILTERED")
        out_dat <- paste0(dataname, "_FILTERED_ALONE")

        filter_call <- private$get_subset_call(dataname, out_dat)
        merge_call <- call("<-", as.name(out),
                           call("merge", x = call("[", as.name("ASL_FILTERED"), quote(expr =), c("USUBJID", "STUDYID")), # nolint
                                y = as.name(out_dat),
                                by = c("USUBJID", "STUDYID"),
                                all.x = FALSE, all.y = FALSE))

        calls <- list(asl_filter_call, filter_call, merge_call)

        if (merge && asl) {
          calls
        } else if (merge && !asl) {
          calls[2:3]
        } else if (!merge && asl) {
          calls[1:2]
        } else {
          calls[[2]]
        }
      }
    },
    hold_filtering = function() {
      private$on_hold <- TRUE
      invisible(NULL)
    },
    continue_filtering = function() {
      private$on_hold <- FALSE
      private$apply_filter("ASL") # rerun all filtering
      invisible(NULL)
    }
  ),

  ## __Private Methods---------------------

  private = list(

    init_datanames = NULL,
    datasets = NULL,
    filtered_datasets = NULL,
    filter_state = NULL,
    filter_info = list(),
    on_hold = FALSE,

    error_if_not_valid = function(dataname, varname=NULL) {

      if (!(dataname %in% self$datanames())) stop(paste("data", dataname, "is not available"))

      if (!is.null(varname) && !(varname %in% names(self$get_data(dataname))))
        stop(paste("variable", varname, "is not in data", dataname))

      NULL
    },

    update_filter_info = function(dataname) {

      df <- self$get_data(dataname)

      fi <- Map(function(var, varname) {
        if (all(is.na(var))) {
          .log("all elements in", varname, "are NA")
          list(
            type = "unknown",
            class = class(var)
          )
        } else if (is.factor(var) || is.character(var)) {
          list(
            type = "choices",
            choices = if(is.factor(var)){
                levels(var)
              }else{
                unique(as.character(var))
              }
          )
        } else if (is.numeric(var)) {
          list(
            type = "range",
            range = range(var, na.rm = TRUE)
          )
        } else if (is.logical(var)) {
          list(
            type = "logical",
            choices = c("TRUE", "FALSE", "TRUE or FALSE")
          )
        } else {
          .log("variable '", varname, "' is of class '",
               class(var), "' which has currently no filter UI element", sep = "")
          list(
            type = "unknown",
            class = class(var)
          )
        }
      }, df, names(df))

      private$filter_info[[dataname]] <- setNames(fi, names(df))

    },

    validate = function() {
      # number of tests to check whether the FilteredData object is consistent

      msg <- NULL
      is_valid <- TRUE



    },

    get_subset_call = function(dataname, out) {

      fs <- isolate(private$filter_state[[dataname]])

      data_filter_call <- if (length(fs) == 0) {

        call("<-", as.name(out), as.name(dataname))

      } else {

        fi <- private$filter_info[[dataname]]
        data_filter_call_items <- Map(function(name) {

          type <- fi[[name]]$type
          state <- fs[[name]]

          if (is.null(type)) stop(paste("filter type for variable", name, "in", dataname, "not known"))

          switch(
            type,
            choices = {
              if (length(state) == 1) {
                call("==", as.name(name), state)
              } else {
                call("%in%", as.name(name), state)
              }
            },
            range   = call("(", call("&", call(">=", as.name(name), state[1]), call("<=", as.name(name), state[2]))),
            logical = {
              switch(
                state,
                "TRUE" = as.name(name),
                "FALSE" = call("!", as.name(name)),
                "TRUE or FALSE" = call("%in%", as.name(name), c(TRUE, FALSE))
              )
            },
            stop(paste("filter type for variable", name, "in", dataname, "not known"))
          )
        }, names(fs))

        condition <- if (length(data_filter_call_items) == 1) {
          data_filter_call_items[[1]]
        } else {
          Reduce(function(x, y) call("&", x, y), data_filter_call_items[-1], init = data_filter_call_items[[1]])
        }

        as.call(list(as.name("<-"), as.name(out), call("subset", as.name(dataname), condition)))
      }

    },

    apply_filter = function(dataname=NULL) {

      if (private$on_hold) return()

      .log("apply filter for", dataname)

      if (is.null(self$get_data("ASL", filtered = FALSE))) {

        # set all filtered datasets to NULL
        for (name in self$datanames())
          private$filtered_datasets[[name]] <- NULL

      } else {

        # filter data directly in an empty environment
        e <- new.env(parent = globalenv())

        # ASL
        e$ASL <- self$get_data("ASL", filtered = FALSE) # nolint

        eval(self$get_filter_call("ASL", merge = FALSE, asl = FALSE), e)

        # re-run all filters
        if (identical(dataname, "ASL")) dataname <- NULL

        if (is.null(dataname)) {

          # filter all except asl
          dnnasl <- setdiff(self$datanames(), "ASL")

          for (name in dnnasl) {

            df <- self$get_data(name, filtered = FALSE)

            if (is.null(df)) {
              e[[paste0(name, "_FILTERED")]] <- NULL
            } else {
              e[[name]] <- self$get_data(name, reactive = FALSE, filtered = FALSE)
              calls <- self$get_filter_call(name, merge = TRUE, asl = FALSE)
              eval(calls[[1]], e)
              eval(calls[[2]], e)
            }
          }

          ## to not trigger multiple events
          private$filtered_datasets[["ASL"]] <- e[["ASL_FILTERED"]]
          for (name in dnnasl) {
            fdn <- paste0(name, "_FILTERED")
            private$filtered_datasets[[name]] <- e[[fdn]]
          }

        } else {
          # filter dataname
          fdn <- paste0(dataname, "_FILTERED")

          df <- self$get_data(dataname, filtered = FALSE)

          if (is.null(df)) {
            e[[FDN]] <- NULL
          } else {
            e[[dataname]] <- df
            calls <- self$get_filter_call(dataname, merge = TRUE, asl = FALSE)
            eval(calls[[1]], e)
            eval(calls[[2]], e)
          }

          private$filtered_datasets[[dataname]] <- e[[fdn]]
        }
        rm(e)
      }
    }
  )
)
