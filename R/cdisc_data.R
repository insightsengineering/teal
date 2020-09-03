#' Data input for teal app
#'
#' Abstract function that creates dataset object with connected metadata.
#' @param dataname (\code{character} value)\cr
#'   name of dataset.
#'
#' @param data (\code{data.frame})\cr
#'   data must contain fields defined in keys.
#'
#' @param label (\code{character})\cr
#'   Label to describe the dataset
#'
#' @param keys (\code{keys})\cr
#'   see \code{\link{keys}}
#' @param label (\code{character} value)\cr
#'   a dataset label
#' @param code (\code{character} value)\cr
#'   code to reproduce \code{data}
#'
#' @param vars (named \code{list})) \cr
#'   In case when this object code depends on the \code{raw_data} from the other
#'   \code{RelationalDataset} object(s) or other constant value,
#'   this/these object(s) should be included as named element of the list.
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
#' ADTTE \code{primary: PARAMCD, foreign: SUBJECT, UNIQUEID, STUDYID, parent: ADSL}
#'
#' As you can see the names of foreign ADTTE are different to ADSL keys, but the order and length is equal.
#'
#' @return a dataset with connected metadata
#'
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' dataset("iris", iris)
dataset <- function(dataname,
                    data,
                    keys = teal::keys(primary = NULL, foreign = NULL, parent = NULL),
                    label = data_label(data),
                    code = character(0),
                    vars = list()) {
  stopifnot(is_character_single(dataname))
  stopifnot(is.data.frame(data))
  stopifnot(is(keys, "keys"))
  stopifnot(all(union(keys$primary, keys$foreign) %in% names(data)))
  stopifnot(is_character_vector(code, min_length = 0, max_length = 1))
  stopifnot(identical(vars, list()) || is_fully_named_list(vars))

  if (!is.null(keys$foreign) && is.null(keys$parent) || (is.null(keys$foreign) && !is.null(keys$parent))) {
    stop(dataname, ": Please specify both foreign keys and a parent!")
  }

  if (!is.null(keys$primary) && any(duplicated(data[, unique(c(keys$primary, keys$foreign))]))) {
    stop(dataname, ": Keys don't uniquely distinguish the rows,  i.e. some rows share the same keys")
  }

  res <- relational_dataset(
    dataname = dataname,
    x = data,
    keys = keys,
    label = label,
    code = code,
    vars = vars
  )

  return(res)
}

#' Load \code{RelationalDataset} object from a file
#'
#' Please note that the script has to end with a call creating desired object. The error will be raised otherwise.
#'
#' @param path (\code{character}) string giving the pathname of the file to read from.
#' @param code (\code{character}) reproducible code to re-create object
#'
#' @return \code{RelationalDataset} object
#'
#' @export
#'
#' @examples
#' # simple example
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'      library(random.cdisc.data)
#'
#'      dataset(dataname = \"ADSL\",
#'              data = radsl(cached = TRUE),
#'              keys = get_cdisc_keys(\"ADSL\"),
#'              code = \"library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)\")"
#'   ),
#'   con = file_example
#' )
#' x <- dataset_file(file_example, code = character(0))
#' get_code(x)
#'
#' # custom code
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'
#'      # code>
#'      library(random.cdisc.data)
#'      adsl <- radsl(cached = TRUE)
#'      adsl$a1 <- 1
#'      adsl$a2 <- 2
#'
#'      # <code
#'      dataset(dataname = \"ADSL\", data = adsl, keys = get_cdisc_keys(\"ADSL\"))"
#'   ),
#'   con = file_example
#' )
#' x <- dataset_file(file_example)
#' get_code(x)
dataset_file <- function(path, code = get_code(path)) {
  relational_dataset_file(path = path, code = code)
}


#' Data input for teal app
#'
#' Function that creates CDISC dataset object
#'
#' @inheritParams dataset
#' @return a dataset with connected metadata
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' cdisc_dataset("ADSL", ADSL)
cdisc_dataset <- function(dataname,
                          data,
                          keys = get_cdisc_keys(dataname),
                          label = data_label(data),
                          code = character(0),
                          vars = list()) {
  dataset(dataname = dataname, data = data, keys = keys, label = label, code = code, vars = vars)
}

#' Load \code{CDISC} \code{RelationalDataset} object from a file
#'
#' Please note that the script has to end with a call creating desired object. The error will be raised otherwise.
#'
#' @inheritParams dataset_file
#'
#' @return \code{RelationalDataset} object
#'
#' @export
#'
#' @examples
#' # simple example
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'      library(random.cdisc.data)
#'
#'      cdisc_dataset(dataname = \"ADSL\",
#'                    data = radsl(cached = TRUE),
#'                    code = \"library(random.cdisc.data)\nADSL <- radsl(cached = TRUE)\")"
#'   ),
#'   con = file_example
#' )
#' x <- cdisc_dataset_file(file_example, code = character(0))
#' get_code(x)
#'
#' # custom code
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(teal)
#'
#'      # code>
#'      library(random.cdisc.data)
#'      adsl <- radsl(cached = TRUE)
#'      adsl$a1 <- 1
#'      adsl$a2 <- 2
#'
#'      # <code
#'      cdisc_dataset(dataname = \"ADSL\", data = adsl)"
#'   ),
#'   con = file_example
#' )
#' x <- cdisc_dataset_file(file_example)
#' get_code(x)
cdisc_dataset_file <- function(path, code = get_code(path)) {
  relational_dataset_file(path = path, code = code)
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

#' Function that returns a keys object
#'
#' @param primary vector of primary key values
#' @param foreign vector of foreign key values
#' @param parent string that indicates parent dataset
#'
#' @return keys
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

  out <- list(primary = primary, foreign = foreign, parent = parent)
  class(out) <- "keys"

  return(out)
}

#' Function that returns the default keys for a `CDISC` dataset by name
#'
#' @md
#' @param dataname name of the `CDISC` dataset
#'
#' @return \code{keys} object
#'
#' @importFrom yaml yaml.load_file
#' @importFrom utils.nest get_package_file
#' @export
#'
#' @examples
#' get_cdisc_keys("ADSL")
get_cdisc_keys <- function(dataname) {
  stopifnot(is_character_single(dataname))

  # copy from excel file
  default_cdisc_keys <- yaml.load_file(get_package_file("teal", "cdisc_datasets/cdisc_datasets.yaml")) #nolint

  if (!(dataname %in% names(default_cdisc_keys))) {
    stop(sprintf("There is no dataset called: %s \n  List of supported cdisc_datasets:\n   %s",
                 dataname, paste(names(default_cdisc_keys), collapse = ", ")))
  } else {
    cdisc_keys <- default_cdisc_keys[[dataname]]

    return(keys(cdisc_keys$primary, cdisc_keys$foreign, cdisc_keys$parent))
  }
}

#' Function that extract labels from CDISC dataset
#'
#' @param data (\code{data.frame}) table to extract the labels from
#' @inheritParams rtables::var_labels
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
#' ADSL <-  radsl(cached = TRUE)
#'
#' get_labels(ADSL)
get_labels <- function(data, fill = TRUE) {
  stopifnot(is.data.frame(data))
  stopifnot(is_logical_single(fill))

  cdisc_labels <- list(
    "dataset_label" = data_label(data),
    "column_labels" = var_labels(data, fill = fill)
  )
  return(cdisc_labels)
}

#' Function that extract column labels from CDISC dataset
#'
#' @param data (\code{data.frame}) any CDISC data set
#' @param columns optional, (\code{character}) column names to extract the labels from. If (\code{NULL}) then all
#'   columns are being used.
#' @inheritParams rtables::var_labels
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
#'
#' ADSL$NEW_COL <- 1
#' get_variable_labels(ADSL, c("AGE", "RACE", "BMRKR1", "NEW_COL"))
#' get_variable_labels(ADSL, c("AGE", "RACE", "BMRKR1", "NEW_COL"), fill = FALSE)
get_variable_labels <- function(data, columns = NULL, fill = TRUE) {
  stopifnot(is.data.frame(data))
  stopifnot(is.null(columns) || is_character_vector(columns))
  stopifnot(is_logical_single(fill))

  columns <- if_null(columns, colnames(data))
  labels <- as.list(get_labels(data, fill = fill)$column_labels)
  # convert NULL into NA_character for not-existing column
  res <- vapply(columns, function(x) if_null(labels[[x]], NA_character_), character(1))

  return(res)
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
#' @param ... (\code{RelationalDataConnector}, \code{RelationalDataset} or
#'   \code{RelationalDatasetConnector}) elements to include where `ADSL` data is mandatory.
#' @param code (\code{character}) code to reproduce the datasets.
#' @param check (\code{logical}) reproducibility check - whether evaluated preprocessing code gives the same objects
#'   as provided in arguments. Check is run only if flag is true and preprocessing code is not empty.
#'
#' @return a \code{RelationalData} object
#'
#' @details This function checks if there were keys added to all data
#'   sets that shall be analyzed inside a teal app.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#'
#' # basic example
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL),
#'   cdisc_dataset("ADTTE", ADTTE),
#'   code = 'ADSL <- radsl(cached = TRUE)
#'           ADTTE <- radtte(cached = TRUE)',
#'   check = TRUE
#' )
#'
#' # Example with keys
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL, keys = keys(
#'     primary = c("STUDYID", "USUBJID"),
#'     foreign = NULL,
#'     parent = NULL),
#'   ),
#'   cdisc_dataset("ADTTE", ADTTE, keys = keys(
#'     primary = c("STUDYID", "USUBJID", "PARAMCD"),
#'     foreign = c("STUDYID", "USUBJID"),
#'     parent = "ADSL"
#'   )),
#'   code = "ADSL <- radsl(cached = TRUE)
#'           ADTTE <- radtte(cached = TRUE)",
#'   check = TRUE
#' )
cdisc_data <- function(...,
                       code = "",
                       check = FALSE) {
  stopifnot(is_logical_single(check))

  x <- teal_data(...)
  if (length(code) > 0 && !identical(code, "")) {
    x$set_pull_code(code = code)
  }
  x$set_check(check)

  datasets_names <- x$get_datanames()
  if (!any(datasets_names == "ADSL")) {
    stop("ADSL argument is missing.")
  }

  if (any(duplicated(datasets_names))) {
    dups <- datasets_names[duplicated(datasets_names)]
    msg <- paste0("Found duplicated dataset names: ", paste0(dups, collapse = ", "), ".")
    stop(msg)
  }

  datasets_keys <- lapply(x$get_items(), function(x) x$get_keys())
  check_foreign_keys(datasets_keys)

  if (check && is_pulled(x)) {
    x$check()
    if (isFALSE(x$get_check_result())) {
      stop("Reproducibility check failed.")
    }
  }

  return(x)
}


#' Load \code{cdisc_data} object from a file
#'
#' @param path A (\code{connection}) or a (\code{character}) string giving the pathname
#'   of the file or URL to read from. "" indicates the connection \code{stdin}.
#'
#' @return \code{cdisc_data} object if file returns a \code{cdisc_data}
#'   object.
#' @export
#'
#' @examples
#' file_example <- tempfile(fileext = ".R")
#' writeLines(
#'   text = c(
#'     "library(random.cdisc.data)
#'
#'      # code>
#'      ADSL <- radsl(cached = TRUE)
#'      ADTTE <- radtte(ADSL, cached = TRUE)
#'
#'      cdisc_data(
#'           cdisc_dataset(\"ADSL\", ADSL), cdisc_dataset(\"ADTTE\", ADTTE),
#'           code = \"ADSL <- radsl(cached = TRUE)
#'                   ADTTE <- radtte(ADSL, cached = TRUE)\",
#'           check = FALSE
#'      )
#'      # <code"
#'   ),
#'   con = file_example
#' )
#'
#' cdisc_data_file(file_example)
#'
#' @importFrom methods is
cdisc_data_file <- function(path) {
  teal_data_file(path)
}
