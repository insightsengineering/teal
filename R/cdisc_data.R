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
                    keys = NULL,
                    labels = NULL) {
  stopifnot(is.character(dataname))
  stopifnot(!is.null(data))

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
  stopifnot(is.character(dataname))

  # copy from excel file
  rel <- list(
    ADSL = list(
      primary = c("STUDYID", "USUBJID"),
      foreign = NULL,
      parent = NULL
    ),
    ADAE = list(
      primary = c("STUDYID", "USUBJID", "ASTDTM", "AETERM", "AESEQ"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADTTE = list(
      primary = c("STUDYID", "USUBJID", "PARAMCD"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADCM = list(
      primary = c("STUDYID", "USUBJID", "ASTDTM", "MHSEQ"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADLB = list(
      primary = c(
        "STUDYID",
        "USUBJID",
        "PARAMCD",
        "BASETYPE",
        "AVISITN",
        "ATPTN",
        "DTYPE",
        "ADTM",
        "LBSEQ",
        "ASPID"
      ),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADRS = list(
      primary = c("STUDYID", "USUBJID", "PARAMCD", "AVISITN", "ADT", "RSSEQ"),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    ),
    ADVS = list(
      primary = c(
        "STUDYID",
        "USUBJID",
        "PARAMCD",
        "BASETYPE",
        "AVISITN",
        "ATPTN",
        "DTYPE",
        "ADTM",
        "VSSEQ",
        "ASPID"
      ),
      foreign = c("STUDYID", "USUBJID"),
      parent = "ADSL"
    )
  )

  if (!(dataname %in% names(rel))) {
    stop(sprintf("There is no dataset called: %s", dataname))
  } else {
    rel[[dataname]]
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
#' @importFrom purrr map_chr
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <-  suppressWarnings(radsl(N = 600, seed = 123))
#'
#' get_cdisc_labels(ADSL)
#'

get_cdisc_labels <- function(data) {
  cdisc_labels <- list(
    "dataset_label" = attr(data, "label"),
    "column_names" = names(data),
    "column_labels" = map_chr(names(data), function(x) {
      if (is.null(attr(data[[x]], "label"))) {
        "Undefined"
      } else {
        attr(data[[x]], "label")
      }
    })
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

cdisc_dataset <-
  function(dataname,
           data,
           keys = get_cdisc_keys(dataname),
           labels = get_cdisc_labels(data)) {
    x <- dataset(dataname, data, keys, labels)
    class(x) <- c("cdisc_dataset", class(x))
    x
  }


###################################### OLD CODE HERE ###########################################

#' Data input for teal app
#'
#' Function passes datasets to teal application with option to read preprocessing code and reproducibility checking.
#' @param ASL ASL dataset
#' @param ... other datasets
#' @param code (\code{NULL} or \code{character}) preprocessing code.
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
#' ASL <-  suppressWarnings(radsl(N = 600, seed = 123))
#' ADTE <- radtte(ASL, event.descr = c("STUDYID", "USUBJID", "PARAMCD"), seed = 123)
#'
#' cdisc_data(
#'   ASL = ASL,
#'   ADTE = ADTE,
#'   code = 'ASL <- radsl(N = 600, seed = 123)
#'           ADTE <- radtte(ASL, event.descr = c("STUDYID", "USUBJID", "PARAMCD"), seed = 123)')
cdisc_data <- function(ASL,
                       # nolint
                       ...,
                       code = NULL,
                       check = FALSE) {
  stopifnot(is.null(code) || is.character.single(code))
  stopifnot(is.logical.single(check))

  if (is.null(code)) {
    code <- ""
  }

  if (missing(ASL)) {
    if (identical(code, "")) {
      stop("ASL and code arguments are missing.")
    } else {
      eval(parse(text = code))
      if (missing(ASL)) {
        stop("ASL is missing and cannot be generated by code.")
      }
    }
  }

  arg_values_call <- append(list("ASL" = substitute(ASL)),
                            as.list(substitute(list(...)))[-1])
  arg_values_char <- sapply(arg_values_call,
                            function(x) {
                              paste0(deparse(x), collapse = "\n")
                            }) %>%
    unname()
  for (i in seq_along(arg_values_call)) {
    if (is.call(arg_values_call[[i]]) &&
        isTRUE(check) && !identical(code, "")) {
      msg <-
        "Automatic checking is not supported if arguments provided as calls."
      stop(msg)
    }
  }

  # eval code if argument does not exists, i.e. cdisc_data(ASL = 1, x, code = "x <- 2")
  for (i in seq_along(arg_values_call)) {
    if ((is.name(arg_values_call[[i]]) ||
         is.call(arg_values_call[[i]])) &&
        inherits(tryCatch(
          eval(arg_values_call[[i]], envir = parent.frame()),
          error = function(e)
            e
        ), "error") &&
        !is.null(code)) {
      eval(parse(text = code), envir = parent.frame())
      break
    }
  }

  arg_values <- setNames(append(list(ASL), list(...)), NULL)

  arg_names <- c("ASL",
                 if (is.null(names(list(...)))) {
                   rep("", length(arg_values) - 1)
                 } else {
                   names(list(...))
                 })

  if (any(arg_names == "")) {
    stop("All arguments passed to '...' should be named.")
  }

  # if user changes variable name via argument
  if (any(arg_names != arg_values_char)) {
    idx <- which(arg_names != arg_values_char)

    msg <- sprintf("Data names should not be changed via argument\n%s",
                   paste(paste0(arg_names[idx], " != ", arg_values_char[idx]),
                         collapse = "\n"))

    stop(msg)
  }

  # if data arguments aren't capitalized
  if (any(arg_names != toupper(arg_names))) {
    idx <- which(arg_names != toupper(arg_names))

    msg <- sprintf(
      "Data arguments should be capitalized. Please change\n%s",
      paste(paste0(
        arg_names[idx], " to ", toupper(arg_names)[idx]
      ),
      collapse = "\n")
    )

    stop(msg)
  }

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

    res_check <- vapply(seq_along(res),
                        function(i,
                                 list,
                                 list_names,
                                 env,
                                 args_call,
                                 args_char) {
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
                        args_char = arg_values_char)

    if (any(!res_check)) {
      incorrect_obj_names <- arg_names[!res_check]
      msg <- paste0("Cannot reproduce object(s) ",
                    paste0(paste0("'", incorrect_obj_names, "'"), collapse = ", "),
                    " based on code.")
      stop(msg)
    }
  }

  if (code == "") {
    package_path <- path.package("teal")
    if ("inst" %in% list.dirs(package_path,
                              full.names = FALSE,
                              recursive = FALSE)) {
      filename <-
        file.path(package_path, "inst", "preprocessing_empty_string.txt")
    } else {
      filename <-
        file.path(package_path, "preprocessing_empty_string.txt")
    }
    code <- readChar(filename, file.info(filename)$size)
  }


  res <- lapply(seq_along(res),
                function(i) {
                  structure(res[[i]], dataname = arg_names[[i]])
                })
  res <- setNames(res, arg_names)

  structure(res, code = code, class = "cdisc_data")
}
