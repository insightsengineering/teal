app_state_grab <- function() {
  session <- .get_session()
  input <- session$input

  ans <- lapply(names(input), function(i) {
    list(id = i, value = as.vector(input[[i]]))
  })

  excluded_ids <- paste(c("filter_panel", "filter_manager", "snapshot_manager", "state_manager"), collapse = "|")
  included_ids <- grep(excluded_ids, vapply(ans, `[[`, character(1L), "id"), value = TRUE, invert = TRUE)
  ans <- Filter(function(x) x[["id"]] %in% included_ids, ans)

  class(ans) <- c("teal_grab", class(ans))

  ans
}

app_state_store <- function(grab, file) {
  checkmate::assert_class(grab, "teal_grab")
  checkmate::assert_path_for_output(file, overwrite = TRUE, extension = "json")

  jsonlite::write_json(jsonlite::serializeJSON(grab, pretty = TRUE), file)
  invisible(NULL)
}

app_state_restore <- function(grab, file) {
  if ((missing(grab) && missing(file)) || (!missing(grab) && !missing(file))) {
    stop("specify either \"grab\" or \"file\"")
  }
  if (!missing(grab)) {
    checkmate::assert_class(grab, "teal_grab")
  }
  if (!missing(file)) {
    checkmate::assert_file_exists(file, access = "r")
  }

  app_state <-
    if (missing(file)) {
      grab
    } else if (missing(grab)) {
      jsonlite::unserializeJSON(jsonlite::read_json(file)[[1L]])
    }

  session <- .get_session()
  input <- session$input

  # validate saved input state
  checkmate::assert_subset(vapply(app_state, `[[`, character(1L), "id"), choices = names(input))

  lapply(app_state, function(i) {
    session$sendInputMessage(inputId = i$id, message = list(value = i$value))
  })

  invisible(NULL)
}


setdiff_teal_grab <- function(x, y) {
  ans <- setdiff(x, y)
  class(ans) <- c("teal_grab", class(ans))
  if (length(ans)) {
    ans
  }
}


format.teal_grab <- function(x) {
  all_ids <- vapply(x, `[[`, character(1), "id")
  all_values <- vapply(x, function(xx) toString(xx[["value"]]), character(1L))

  contents <- if (length(all_ids) + length(all_values) > 0L) {
    all_values_trimmed <- lapply(all_values, function(x) {
      if (nchar(x) > 40) {
        paste(substr(x, 1, 36),  "...")
      } else {
        x
      }
    })
    longest_id <- max(nchar(all_ids))
    longest_value <- max(nchar(all_values_trimmed))
    sprintf(sprintf("%%0%ds : %%0%ds", longest_id + 2L, longest_value), all_ids, all_values_trimmed)
  } else {
    "  no inputs"
  }

  paste(
    c(
      "teal_grab:",
      contents,
      ""
    ),
    collapse = "\n"
  )
}


print.teal_grab <- function(x, ...) {
  cat(format(x, ...))
}


.get_session <- function() {
  local_session <- shiny::getDefaultReactiveDomain()
  app_session <- .subset2(local_session, "parent")
  if (is.null(app_session)) {
    local_session
  } else {
    app_session
  }
}
