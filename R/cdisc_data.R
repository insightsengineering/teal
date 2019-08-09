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
#'

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

  if (!is.null(keys$primary) && any(duplicated(data[, keys$primary]))) {
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
#' get_keys(primary = c("STUDYID"), foreign = c("USUBJID"), "ADSL")
#'

get_keys <- function(primary, foreign, parent) {

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
#'

get_cdisc_keys <- function(dataname) {
  stopifnot(is.character.single(dataname))

  # copy from excel file
  default_cdisc_keys <- list(
    ADSL = get_keys(
      primary = c("STUDYID", "USUBJID"),
      foreign = NULL,
      parent = NULL
    ),
    ADAE = get_keys(
      primary = c("STUDYID", "USUBJID"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADTTE = get_keys(
      primary = c("STUDYID", "USUBJID", "PARAMCD"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADCM = get_keys(
      primary = c("STUDYID", "USUBJID"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADLB = get_keys(
      primary = c(
        "STUDYID",
        "USUBJID",
        "PARAMCD"
      ),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADRS = get_keys(
      primary = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADVS = get_keys(
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
#' @importFrom tern var_labels
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <-  suppressWarnings(radsl(N = 600, seed = 123))
#'
#' get_labels(ADSL)
#'

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
#'

cdisc_dataset <- function(dataname,
                          data,
                          keys = get_cdisc_keys(dataname),
                          labels = get_labels(data)) {

    x <- dataset(dataname, data, keys, labels)
    class(x) <- c("cdisc_dataset", class(x))
    x
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
#'   cdisc_dataset("ADSL", ADSL, keys = list(
#'     primary = c("STUDYID", "USUBJID"),
#'     foreign = NULL,
#'     parent = NULL
#'   )),
#'   cdisc_dataset("ADTTE", ADTTE, keys = list(
#'     primary = c("STUDYID", "USUBJID", "PARAMCD"),
#'     foreign = c("STUDYID", "USUBJID"),
#'     parent = "ADSL"
#'   )),
#'   code = "ADSL <- radsl(N = 600, seed = 123)
#'           ADTTE <- radtte(ADSL, seed = 123)",
#'   check = FALSE
#' )
#'
#'
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

  for (keys in datasets_keys) {
    if (!is.null(keys$parent)){
      if (any(!(keys$foreign %in% names(datasets_data[[keys$parent]])))){
        stop("Specified foreign keys are not exisiting in parent dataset.")
      }
    }
  }

  arg_names <- lapply(
    as.list(substitute(list(...)))[-1L],
    function(i) {
      deparse(as.list(match.call(eval(i[[1L]]), i))$data)
    }
  )

  if (check) {
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

  res <- lapply(seq_along(dlist),
                function(i) {
                  structure(dlist[[i]])
                })

  res <- setNames(res, datasets_names)

  structure(res, code = code, class = "cdisc_data")
}
