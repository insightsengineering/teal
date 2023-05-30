#' Generates header text for analysis items
#'
#' @param title A character title of the module
#' @param description A character description of the module with additional
#'   information not reflected in the title
#'
#' @return A character string for the header text
#'
#' @author Sebastian Wolf
#' @keywords internal
get_rcode_header <- function(title = NULL, description = NULL) {
  # Derive sys Info
  info <- Sys.info()
  packages <- sapply(utils::sessionInfo()$otherPkgs, function(x) sprintf("%s (%s)", x$Package, x$Version))
  head <-
    c(
      pad(title, pre = "", post = ""),
      pad(description, post = c("", "")),
      pad(
        c(
          paste("  Running:", getwd()),
          paste("       on:", info["nodename"]),
          paste("R version:", utils::sessionInfo()[["R.version"]][["version.string"]]),
          paste("     Date:", date())
        )
      ),
      "Current libraries loaded (derived by .libPaths()):",
      paste0("  - ", .libPaths()),
      "",
      fold_lines(paste("Packages versions:", paste(packages, collapse = ", ")), 80, indent_from = ":")
    )
  paste0("# ", head)
}

#' Generates library calls from current session info
#'
#' Function to create multiple library calls out of current session info to make reproducible code works.
#'
#' @return Character object contain code
#' @keywords internal
get_rcode_libraries <- function() {
  vapply(
    utils::sessionInfo()$otherPkgs,
    function(x) {
      paste0("library(", x$Package, ")")
    },
    character(1)
  ) %>%
    # put it into reverse order to correctly simulate executed code
    rev() %>%
    paste0(sep = "\n") %>%
    paste0(collapse = "")
    # motivation: second one does not have '\n' at the end which is needed
    # letters[1:5] %>% paste0(sep = "\n") %>% paste0(collapse = "")
    # letters[1:5] %>% paste0(collapse = "\n")
}



get_rcode_str_install <- function() {
  code_string <- getOption("teal.load_nest_code")

  if (!is.null(code_string) && is.character(code_string)) {
    return(code_string)
  }

  return("# Add any code to install/load your NEST environment here\n")
}

#' Pads a string
#'
#' Including elements before or after string. If NULL is provided no elements included. Padding in this case means
#' appending additional element before or after \code{character} vector.
#' @param str (\code{character}) vector of lines to be padded
#' @param pre (\code{character}) elements to be appended before \code{str}
#' @param post (\code{character}) elements to be appended after \code{str}
#'
#' @keywords internal
pad <- function(str, pre = NULL, post = "") {
  if (length(str) == 0 || (length(str) == 1 && str == "")) {
    NULL
  } else {
    c(pre, str, post)
  }
}

#' Fixed line width folding
#'
#' @description `r lifecycle::badge("stable")`
#' Folds lines longer than specified width.
#' @param txt (\code{character}) text to be adjusted
#' @param width (\code{integer}) maximum number of characters in vector
#' @param indent_from (\code{character}) character which begins the indent.
#' @keywords internal
fold_lines <- function(txt, width = 80, indent_from = NULL) {
  unlist(sapply(txt, USE.NAMES = FALSE, FUN = function(x) {
    if (nchar(x) < width) {
      return(x)
    }
    idx <- if (!is.null(indent_from)) {
      gregexpr(indent_from, x)[[1]]
    } else {
      0
    }
    strwrap(
      x = x,
      width = width,
      prefix = strrep(" ", idx + 1),
      initial = ""
    )
  }))
}
