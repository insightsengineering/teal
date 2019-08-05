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


  if (any(!(union(keys$primary, keys$foreign) %in% names(data)))) {
    stop(sprintf("Dataset does not contain column(s) specified as keys"))
  }

  if (any(!(names(labels$column_labels) %in% names(data)))) {
    stop(sprintf("Dataset does not contain column(s) specified as labels"))
  }

  structure(list(
    dataname = dataname,
    data = data,
    keys = keys,
    labels = labels
  ),
  class = c("dataset"))

}

#' Data input for teal app
#'
#' Function that creates CDISC dataset object
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
    ADSL = list(
      primary = c("STUDYID", "USUBJID"),
      foreign = NULL,
      parent = NULL
    ),
    ADAE = list(
      primary = c("STUDYID", "USUBJID"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADTTE = list(
      primary = c("STUDYID", "USUBJID", "PARAMCD"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADCM = list(
      primary = c("STUDYID", "USUBJID"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADLB = list(
      primary = c(
        "STUDYID",
        "USUBJID",
        "PARAMCD"
      ),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADRS = list(
      primary = c("STUDYID", "USUBJID", "PARAMCD"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADVS = list(
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

#' Data input for teal app
#'
#' Function that extract labels from CDISC dataset
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
#' @param ADSL ADSL dataset object
#' @param ... other datasets objects
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
#' ADSL <-  suppressWarnings(radsl(N = 600, seed = 123))
#' ADTTE <- radtte(ADSL, event.descr = c("STUDYID", "USUBJID", "PARAMCD"), seed = 123)
#'
#' cdisc_data(
#'   cdisc_dataset("ADSL", ADSL),
#'   cdisc_dataset("ADTTE", ADTTE),
#'   code = 'ADSL <- radsl(N = 600, seed = 123)
#'           ADTTE <- radtte(ADSL, event.descr = c("STUDYID", "USUBJID", "PARAMCD"), seed = 123)')
cdisc_data <- function(ADSL, # nolint
                       ...,
                       code = "",
                       check = FALSE) {
  stopifnot(is.character.vector(code))
  stopifnot(is.logical.single(check))

  code <- paste0(code, collapse = "\n")

  if (missing(ADSL)) {
    if (identical(code, "")) {
      stop("ADSL and code arguments are missing.")
    } else {
      eval(parse(text = code))
      if (missing(ADSL)) {
        stop("ADSL is missing and cannot be generated by code.")
      }
    }
  }


  ADSL <- ADSL$data #nolint

  dlist <- lapply(list(...), function(x) {
      x$data
    })
  names(dlist) <- lapply(list(...), function(x) {
      x$dataname
    })

  arg_values_call <- append(
    list("ADSL" = substitute(ADSL)),
    as.list(substitute(dlist))
  )
  arg_values_char <- sapply(
    arg_values_call,
    function(x) {
      paste0(deparse(x), collapse = "\n")
    }
  ) %>%
    unname()

  # eval code if argument does not exists, i.e. cdisc_data(ADSL = 1, x, code = "x <- 2")
  for (i in seq_along(arg_values_call)) {
    if ((is.name(arg_values_call[[i]]) || is.call(arg_values_call[[i]])) &&
        inherits(tryCatch(eval(arg_values_call[[i]], envir = parent.frame()), error = function(e) e), "error") &&
        !is.null(code)) {
      eval(parse(text = code), envir = parent.frame())
      break
    }
  }

  arg_values <- setNames(append(list(ADSL), dlist), NULL)

  arg_names <- c(
    "ADSL",
    if (is.null(names(dlist))) {
      rep("", length(arg_values) - 1)
    } else {
      names(dlist)
    }
  )

  res <- setNames(arg_values, arg_names)

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
      seq_along(res),
      function(i, list, list_names, env, args_call, args_char) {
        list_obj_name <- list_names[i]
        env_obj_name <- if (is.name(args_call[[i]])) {
          args_char[i]
        } else {
          list_names[i]
        }
        tryCatch({
          identical(list[[list_obj_name]], get(env_obj_name, envir = env))
        }, error = function(e) {
          FALSE
        })
      },
      logical(1),
      list = res,
      list_names = arg_names,
      env = new_env,
      args_call = arg_values_call,
      args_char = arg_values_char
    )

    if (any(!res_check)) {
      incorrect_obj_names <- arg_names[!res_check]
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
    seq_along(res),
    function(i) {
      structure(res[[i]], dataname = arg_names[[i]])
    }
  )
  res <- setNames(res, arg_names)

  structure(res, code = code, class = "cdisc_data")
}
