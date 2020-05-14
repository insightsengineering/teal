#' Data input for teal app
#'
#' Abstract function that creates dataset object with connected metadata.
#' @param dataname name of dataset
#' @param data data
#' @param keys list of keys -
#'
#' Please note that the order of keys is important.
#'
#' Keys are not matched by name but by order. In case you want to perform:
#'
#' \code{SELECT ...}
#' \code{  FROM ADSL}
#' \code{LEFT JOIN ADTTE on ADTTE.STUDYID = ADSL.STUDYID and}
#' \code{                                     ADTTE.SUBJECTUNIQUEID = ADSL.USUBJID}
#'
#' the keys need to be
#'
#' ADSL \code{primary: USUBJID, STUDYID}
#'
#' ADTTE \code{primary: PARAMCD, foreign: SUBJECTUNIQUEID, STUDYID, parent: ADSL}
#'
#' As you can see the names of foreign ADTTE are different to ADSL keys, but the order and length is equal.
#'
#' @return a dataset with connected metadata
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <-  suppressWarnings(radsl(N = 600, seed = 123))
#'
#' dataset("ADSL", ADSL)
dataset <- function(dataname,
                    data,
                    keys = list(primary = NULL, foreign = NULL, parent = NULL)) {
  stopifnot(is_character_single(dataname))
  stopifnot(is.data.frame(data))
  stopifnot(is.list(keys))
  stopifnot(all_true(keys, function(x) is.null(x) || is_character_vector(x)))
  stopifnot(all(c("primary", "foreign", "parent") %in% names(keys)))
  stopifnot(all(union(keys$primary, keys$foreign) %in% names(data)))

  if (!is.null(keys$foreign) && is.null(keys$parent) || (is.null(keys$foreign) && !is.null(keys$parent))) {
    stop(dataname, ": Please specify both foreign keys and a parent!")
  }

  if (!is.null(keys$primary) && any(duplicated(data[, unique(c(keys$primary, keys$foreign))]))) {
    stop(dataname, ": Keys don't uniquely distinguish the rows,  i.e. some rows share the same keys")
  }

  res <- structure(
    list(
      dataname = dataname,
      data = data,
      keys = keys,
      column_labels = var_labels(data),
      dataset_label = data_label(data)
    ),
    class = "dataset"
  )

  return(res)
}

#' Get dataset label attribute
#'
#' @param data \code{data.frame} from which attribute is extracted
#'
#' @return (\code{character}) label or \code{NULL} if it's missing
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' data_label(radsl(cached = TRUE))
data_label <- function(data) {
  attr(data, "label")
}

#' Set dataset label attribute
#'
#' @param x \code{data.frame} for which attribute is set
#' @param value (\code{character}) label
#'
#' @return modified \code{x} object
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' x <- radsl(cached = TRUE)
#' data_label(x) <- "My custom label"
#' data_label(x)
`data_label<-` <- function(x, value) { # nolint
  stopifnot(is.data.frame(x))
  stopifnot(is_character_single(value))

  attr(x, "label") <- value
  x
}

#' Function that returns list of keys
#'
#' @param primary vector of primary key values
#' @param foreign vector of foreign key values
#' @param parent string that indicates parent dataset
#'
#' @return list of keys
#'
#' @export
#'
#' @examples
#'
#' keys(primary = c("STUDYID"), foreign = c("USUBJID"), "ADSL")
keys <- function(primary, foreign, parent) {
  stopifnot(is.null(primary) || is_character_vector(primary))
  stopifnot(is.null(foreign) || is_character_vector(foreign))
  stopifnot(is.null(parent) || is_character_single(parent))

  return(list(primary = primary, foreign = foreign, parent = parent))
}

#'
#' Function that creates keys object
#' @param dataname name of dataset
#'
#' @return keys
#'
#' @export
#'
#' @importFrom yaml yaml.load_file
#'
#' @examples
#'
#' get_cdisc_keys("ADSL")
get_cdisc_keys <- function(dataname) {
  stopifnot(is_character_single(dataname))

  # copy from excel file
  filename <- system.file(file.path("cdisc_datasets", "cdisc_datasets.yaml"), package = "teal")
  stopifnot(file.exists(filename))
  default_cdisc_keys <- yaml.load_file(filename)

  if (!(dataname %in% names(default_cdisc_keys))) {
    stop(sprintf("There is no dataset called: %s \n  List of supported cdisc_datasets:\n   %s",
                 dataname, paste(names(default_cdisc_keys), collapse = ", ")))
  } else {
    return(default_cdisc_keys[[dataname]])
  }
}

#' Function that extract labels from CDISC dataset
#'
#' @param data data
#'
#' @return labels
#'
#' @export
#'
#' @importFrom rtables var_labels
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <-  suppressWarnings(radsl(N = 600, seed = 123))
#'
#' get_labels(ADSL)
get_labels <- function(data) {
  stopifnot(is.data.frame(data))

  cdisc_labels <- list(
    "dataset_label" = data_label(data),
    "column_labels" = var_labels(data, fill = TRUE)
  )
  return(cdisc_labels)
}

#' Function that extract column labels from CDISC dataset
#'
#' @param data (\code{data.frame}) Any CDISC data set
#' @param columns (\code{character}) Column names to extract the labels
#'   from
#'
#' @return labels of the columns
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#'
#' get_variable_labels(ADSL)
#' get_variable_labels(ADSL, c("AGE", "RACE", "BMRKR1"))
#' get_variable_labels(ADSL, c("AGE", "RACE", "BMRKR1", "xyz"))
get_variable_labels <- function(data, columns = NULL) {
  stopifnot(is.data.frame(data))
  stopifnot(is.null(columns) || is_character_vector(columns))

  columns <- if_null(columns, colnames(data))
  labels <- as.list(get_labels(data)$column_labels)
  res <- vapply(columns, function(x) if_null(labels[[x]], ""), character(1))

  return(res)
}

#' Data input for teal app
#'
#' Function that creates CDISC dataset object
#'
#' List of implemented cdisc datasets:
#'
#' \itemize{
#'   \item ADSL
#'   \item ADTTE
#'   \item ADAE
#'   \item ADLB
#'   \item ADCM
#'   \item ADRS
#'   \item ADVS
#' }
#'
#' @inheritParams dataset
#' @return a dataset with connected metadata
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <-  suppressWarnings(radsl(N = 600, seed = 123))
#'
#' cdisc_dataset("ADSL", ADSL)
cdisc_dataset <- function(dataname,
                          data,
                          keys = get_cdisc_keys(dataname)) {
    x <- dataset(dataname, data, keys)
    class(x) <- c("cdisc_dataset", class(x))
    x
  }


#' Utility function to check if foreign keys are existing in parent dataset
#'
#' @param datasets_keys list of keys
#'
#' @return NULL
check_foreign_keys <- function(datasets_keys) {
  lapply(datasets_keys, function(keys) {
    if (is.null(keys)) {
      return(invisible(NULL))
    }
    # CHILD TO PARENT
    if (!is.null(keys$parent)) {

      child_keys <- keys$foreign
      parent_keys <- datasets_keys[[keys$parent]]$primary

      if (is.null(datasets_keys[[keys$parent]])) {
        stop("Parent dataset '", keys$parent, " doesn't exists.")
      }

      # one to one OK - identical keys
      if (identical(child_keys, parent_keys)) {
        # do nothing

      # one to many OK - eg. STUDIES to ADSL on STUDYID
      } else if (length(child_keys) < length(parent_keys) && all(child_keys %in% parent_keys)) {
        # do nothing

      # one to one OK - allow to merge on different names eg. USUBJID = ADSL_USUBJID, STUDYID = ADSL_STUDYID
      } else if (length(child_keys) == length(parent_keys) && any(child_keys != parent_keys)) {
        warning(
          "Following foreign keys are not identical to the primary keys in parent dataset:\n",
          paste(paste("  ", child_keys, "!=", parent_keys)[child_keys != parent_keys], collapse = "\n")
        )

      # many to one ERROR - more keys in child than in parent
      } else if (length(child_keys) > length(parent_keys)) {
        stop(
          "Number of foreign keys can't be larger than number of primary keys in parent dataset",
          "\n    Child keys:  ", paste(child_keys, collapse = " "),
          "\n    Parent keys: ", paste(parent_keys, collapse = " "),
          "\nConsider adding ", paste(sprintf("'%s'", setdiff(child_keys, parent_keys)), collapse = ", "),
          " to the parent primary keys or remove from child foreign keys"
        )

      # one to many ERROR - can't be joined if keys doesn't match (eg. STUDIES to ADSL on s.ID = a.STUDYID)
      } else if (length(child_keys) < length(parent_keys) && !all(child_keys %in% parent_keys)) {
        stop(
          "Foreign keys are don't match all parent keys and both have different length",
          "\n    Child keys:  ", paste(child_keys, collapse = " "),
          "\n    Parent keys: ", paste(parent_keys, collapse = " "),
          "\nConsider adding ", paste(sprintf("'%s'", setdiff(child_keys, parent_keys)), collapse = ", "),
          " to the parent primary keys or remove from child foreign keys"
        )
      }

    # root dataset parent mean  - keys must match ADSL keys
    } else {
      if (!identical(keys$primary, datasets_keys[["ADSL"]]$primary))
        stop("Root dataset keys doesn't match ADSL primary keys
             \nADSL keys: ", paste(datasets_keys[["ADSL"]]$primary, collapse = ", "),
             "\nroot keys: ", paste(keys$primary, collapse = ", "))
    }
  })

  return(invisible(NULL))
}

#' Data input for teal app
#'
#' Function passes datasets to teal application with option to read preprocessing code and reproducibility checking.
#' @param ... datasets objects
#' @param code (\code{character}) preprocessing code.
#' @param check (\code{logical}) reproducibility check - whether evaluated preprocessing code gives the same objects
#'   as provided in arguments. Check is run only if flag is true and preprocessing code is not empty.
#'
#' @return a list of the input data sets
#'
#' @details This function checks if there were keys added to all data
#'   sets that shall be analyzed inside a teal app.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl # or: radsl(N = 600, seed = 123)
#' ADTTE <- cadtte # or: radtte(ADSL, seed = 123)
#'
#' # basic example
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL),
#'   cdisc_dataset("ADTTE", ADTTE),
#'   code = 'ADSL <- cadsl
#'           ADTTE <- cadtte')
#'
#' # Example with keys
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL, keys = keys(
#'     primary = c("STUDYID", "USUBJID"),
#'     foreign = NULL,
#'     parent = NULL
#'   )),
#'   cdisc_dataset("ADTTE", ADTTE, keys = keys(
#'     primary = c("STUDYID", "USUBJID", "PARAMCD"),
#'     foreign = c("STUDYID", "USUBJID"),
#'     parent = "ADSL"
#'   )),
#'   code = "ADSL <- radsl(N = 600, seed = 123)
#'           ADTTE <- radtte(ADSL, seed = 123)",
#'   check = FALSE
#' )
cdisc_data <- function(...,
                       code = "",
                       check = FALSE) {
  stopifnot(is_character_vector(code))
  stopifnot(is_logical_single(check))

  code <- paste0(code, collapse = "\n")
  dlist <- list(...)

  if (!is_class_list("dataset")(dlist)) {
    stop("Argument in not of class dataset, please use cdisc_dataset function!")
  }

  datasets_names <- lapply(dlist, `[[`, "dataname")
  datasets_data <- lapply(dlist, `[[`, "data")
  datasets_data <- setNames(datasets_data, datasets_names)
  datasets_keys <- lapply(dlist, `[[`, "keys")
  datasets_keys <- setNames(datasets_keys, datasets_names)

  if (!any(datasets_names == "ADSL")) {
    stop("ADSL argument is missing.")
  }
  if (any(duplicated(datasets_names))) {
    dups <- datasets_names[duplicated(datasets_names)]
    msg <- paste0("Found duplicated dataset names: ", paste0(dups, collapse = ", "), ".")
    stop(msg)
  }

  check_foreign_keys(datasets_keys)

  if (check) {
    arg_names <- lapply(
      as.list(substitute(list(...)))[-1L],
      function(i) {
        tryCatch(
          deparse(as.list(match.call(eval(i[[1L]]), i))$data),
          error = function(e) {
            i[["dataname"]]
          }
        )
      }
    )

    if (identical(code, "")) {
      stop("Cannot check preprocessing code - code is empty.")
    }

    new_env <- new.env(parent = parent.env(.GlobalEnv))
    tryCatch({
      eval(parse(text = code), new_env)
    }, error = function(e) {
      error_dialog(e)
    })

    res_check <- vapply(
      seq_along(datasets_data),
      function(i, list, list_names, arg_names, env) {
        list_obj_name <- list_names[i]
        env_obj_name <- arg_names[i]
        tryCatch({
          identical(list[[list_obj_name]], get(env_obj_name, envir = env))
        }, error = function(e) {
          FALSE
        })
      },
      logical(1),
      list = datasets_data,
      list_names = unlist(datasets_names),
      arg_names = unlist(arg_names),
      env = new_env
    )

    if (any(!res_check)) {
      incorrect_obj_names <- datasets_names[!res_check]
      msg <- paste0(
        "Cannot reproduce object(s) ",
        paste0(paste0("'", incorrect_obj_names, "'"), collapse = ", "),
        " based on code."
      )
      stop(msg)
    }
  }

  if (code == "") {
    code <- get_preprocessing_empty_string() # nolint
  }

  if (!check && !isTRUE(attr(check, "quiet"))) {
    check_note <- get_check_note_string() # nolint
    code <- paste0(code, "\n\n", check_note, "\n")
  }

  res <- lapply(
    seq_along(dlist),
    function(i) {
      structure(dlist[[i]])
    })

  res <- setNames(res, datasets_names)

  return(structure(res, code = code, class = "cdisc_data"))
}


get_preprocessing_empty_string <- function() {
  filename <- get_package_file("preprocessing_empty_string.txt") # nolint
  readChar(filename, file.info(filename)$size)
}


get_check_note_string <- function() {
  filename_check <- get_package_file("check_false_string.txt") # nolint
  readChar(filename_check, file.info(filename_check)$size)
}

get_package_file <- function(file_name) {
  package_path <- path.package("teal")
  if ("inst" %in% list.dirs(package_path, full.names = FALSE, recursive = FALSE)) {
    file.path(package_path, "inst", file_name)
  } else {
    file.path(package_path, file_name)
  }
}
