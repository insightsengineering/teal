#' Data input for teal app
#'
#' Abstract function that creates dataset object with connected metadata.
#' @param dataname name of dataset
#' @param data data
#' @param keys list of keys
#' @param labels list of labels
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
                    keys = list(primary = NULL, foreign = NULL, parent = NULL),
                    labels = list(dataset_label = NULL, column_labels = NULL)) {

  stopifnot(is.character.single(dataname))
  stopifnot(is.data.frame(data))
  stopifnot(is.list(keys))
  stopifnot(all_true(keys, function(x) is.null(x) || is.character.vector(x)))
  stopifnot(all(c("primary", "foreign", "parent") %in% names(keys)))
  stopifnot(is.list(labels))
  stopifnot(all_true(labels, function(x) is.null(x) || (is.character(x) || is.character.vector(x))))
  stopifnot(all(c("dataset_label", "column_labels") %in% names(labels)))
  stopifnot(all(union(keys$primary, keys$foreign) %in% names(data)))
  stopifnot(all(names(labels$column_labels) %in% names(data)))

  if (!is.null(keys$foreign) && is.null(keys$parent) || (is.null(keys$foreign) && !is.null(keys$parent))) {
    stop(dataname, ": Please specify both foreign keys and a parent!")
  }

  if (!is.null(keys$primary) && any(duplicated(data[, unique(c(keys$primary, keys$foreign))]))) {
    stop(dataname, ": Keys don't uniquely distinguish the rows,  i.e. some rows share the same keys")
  }

  structure(list(
    dataname = dataname,
    data = data,
    keys = keys,
    labels = labels
  ),
  class = c("dataset"))

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

  stopifnot(is.null(primary) || is.character.vector(primary))
  stopifnot(is.null(foreign) || is.character.vector(foreign))
  stopifnot(is.null(parent) || is.character.single(parent))

  list(primary = primary, foreign = foreign, parent = parent)
}

#'
#' Function that creates keys object
#' @param dataname name of dataset
#'
#' @return keys
#'
#' @export
#'
#' @examples
#'
#' get_cdisc_keys("ADSL")
get_cdisc_keys <- function(dataname) {
  stopifnot(is.character.single(dataname))

  # copy from excel file
  default_cdisc_keys <- list(
    ADSL = keys(
      primary = c("STUDYID", "USUBJID"),
      foreign = NULL,
      parent = NULL
    ),
    ADAE = keys(
      primary = c("STUDYID", "USUBJID", "ASTDTM", "AETERM", "AESEQ"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADTTE = keys(
      primary = c("STUDYID", "USUBJID", "PARAMCD"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADCM = keys(
      primary = c("STUDYID", "USUBJID"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADLB = keys(
      primary = c(
        "STUDYID",
        "USUBJID",
        "PARAMCD",
        "AVISIT"
      ),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADRS = keys(
      primary = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADVS = keys(
      primary = c(
        "STUDYID",
        "USUBJID",
        "PARAMCD"
      ),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    )
  )

  if (!(dataname %in% names(default_cdisc_keys))) {
    stop(sprintf("There is no dataset called: %s", dataname))
  } else {
    default_cdisc_keys[[dataname]]
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
    "dataset_label" = attr(data, "label"),
    "column_labels" = var_labels(data, fill = TRUE)
  )
  cdisc_labels
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
#' @param dataname name of dataset
#' @param data data
#' @param keys list of keys
#' @param labels list of labeles
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
#' cdisc_dataset("ADSL", ADSL)
cdisc_dataset <- function(dataname,
                          data,
                          keys = get_cdisc_keys(dataname),
                          labels = get_labels(data)) {

    x <- dataset(dataname, data, keys, labels)
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
#' ADSL <-  radsl(N = 600, seed = 123)
#' ADTTE <- radtte(ADSL, seed = 123)
#'
#' # basic example
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL),
#'   cdisc_dataset("ADTTE", ADTTE),
#'   code = 'ADSL <- radsl(N = 600, seed = 123)
#'           ADTTE <- radtte(ADSL, seed = 123)')
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
  stopifnot(is.character.vector(code))
  stopifnot(is.logical.single(check))

  code <- paste0(code, collapse = "\n")
  dlist <- list(...)

  if (!is.class.list("dataset")(dlist)) {
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
          error = function(e){
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
      stop(paste0("Error in checking code: ", e$message))
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
    package_path <- path.package("teal")
    if ("inst" %in% list.dirs(package_path, full.names = FALSE, recursive = FALSE)) {
      filename <- file.path(package_path, "inst", "preprocessing_empty_string.txt")
    } else {
      filename <- file.path(package_path, "preprocessing_empty_string.txt")
    }
    code <- readChar(filename, file.info(filename)$size)
  }

  res <- lapply(
    seq_along(dlist),
    function(i) {
      structure(dlist[[i]])
    })

  res <- setNames(res, datasets_names)

  structure(res, code = code, class = "cdisc_data")
}
