merge_selectors_srv <- function(...,
                        join_keys = teal.data::join_keys(),
                        output_dataname = "anl",
                        join_fun = "dplyr::full_join",
                        session = shiny::getDefaultReactiveDomain()) {
  reactive_merge_output <- reactive({
    merge_selectors(..., join_keys = join_keys, output_dataname = output_dataname, join_fun = join_fun)
  })

  # set dummy inputs values with values set to reactive_merge_output()$selectors
  # each named by selector name
  observeEvent(
    reactive_merge_output(),
    {
      selectors <- reactive_merge_output()$selectors
      for (i in seq_along(selectors)) {
        input_name <- sprintf("input$%s_prefixed", names(selectors)[i])
        input_value <- selectors[[i]]
        #session$sendCustomMessage
        # todo: register new input
        #       or do js script to add attributes to the original input (dataname and prefixed)
      }
    }
  )

  reactive(reactive_merge_output()$expr)

}

#' Make a merge expression
#'
#' This function takes multiple selectors and join keys to create a merge expression.
#' Returned call is different based on number and type of arguments:
#' - Selectors from the same datanames are grouped together and producing a single `dplyr::select` call.
#' - Merge call is added if multiple datanames are involved.
#' - Datasets are merged `by` respective join keys.
#' - if there are duplicated colnames across datanames then [dplyr::join] prefixes them `<dataname>.<colname>`
#'
#' In order to address prefixing of the duplicated columns original selectors are also prefixed. This means
#' that original selection-name is no longer valid and should be replaced with the new one.
#'
#' @examples
#' # example input (user's input selection)
#' input <- list(
#'   x = c("col1", "col2"),
#'   y = c("col3", "col4"),
#'   z = c("col3", "col5")
#' )
#'
#' selector_x <- structure(input$x, dataname = "dataname1")
#' selector_y <- structure(input$y, dataname = "dataname2")
#' selector_z <- structure(input$z, dataname = "dataname3")
#'
#' jk <- teal.data::join_keys(
#'   join_key("dataname1", "dataname2", c(id = "parent_id")),
#'   join_key("dataname1", "dataname3", c(id = "parent_id"))
#' )
#'
#' merge_selectors(x = selector_x, y = selector_y, z = selector_z, join_keys = jk)
merge_selectors <- function(...,
                            join_keys = teal.data::join_keys(),
                            output_dataname = "anl",
                            join_fun = "dplyr::full_join") {
  # [Q]: should we prefix all colnames if there is merge involved or only duplicated ones?
  #   - app developer could expect <selector>.<colname> in advance
  selectors <- list(...)
  selectors_datanames <- vapply(selectors, FUN.VALUE = character(1), FUN = function(x) {
    dataname <- attr(x, "dataname")
    if (is.null(dataname)) {
      stop("dataname attribute is required")
    }
    dataname
  })

  colnames_by_datanames <- tapply(selectors, selectors_datanames, function(x) {
    unique(unclass(unlist(x)))
  })
  datanames <- names(colnames_by_datanames)

  merge_call <- make_select_call(
    dataname = datanames[1],
    colnames = unique(
      c(
        unname(unlist(join_keys[datanames[1]])), # all foreign and primary keys
        colnames_by_datanames[[1]]
      )
    )
  )

  if (length(datanames) > 1) {
    # merge call concatenated by %>%
    for (i in 2:length(datanames)) {
      join_key_pair <- join_keys[datanames[i-1], datanames[i]]
      join_call <- as.call(
        c(
          str2lang(join_fun),
          make_select_call(
            dataname = datanames[[i]],
            colnames = unique(
              c(
                unname(unlist(join_keys[datanames[i]])), # all foreign and primary keys
                colnames_by_datanames[[i]]
              )
            )
          ),
          if (length(join_key_pair)) list(by = join_key_pair)
        )
      )
      merge_call <- as.call(c(quote(`%>%`), merge_call, join_call))
    }

    # if there are duplicated colnames across selectors
    # then merge function prefixes them <dataname>.<colname>
    duplicated_colnames_idx <- duplicated(unlist(colnames_by_datanames))
    duplicated_colnames <- unique(unlist(colnames_by_datanames)[duplicated_colnames_idx])
    selectors <- lapply(list(...), function(x) {
      dataname_i <- attr(x, "dataname")
      colnames_i <- unclass(x)
      is_prefixed <- colnames_i %in% duplicated_colnames
      if (any(is_prefixed)) {
        colnames_i[is_prefixed] <- sprintf("%s.%s", dataname_i, colnames_i[is_prefixed])
      }
      structure(colnames_i, dataname = dataname_i)
    })
  }

  merge_call <- call(
    "<-",
    as.name(output_dataname),
    merge_call
  )


  list(
    # todo: instead of returning selectors from here it should register
    # input in the module as input$<selector name>-prefixed
    selectors = selectors,
    expr = merge_call
  )
}

make_select_call <- function(dataname, colnames) {
  as.call(c(quote(dplyr::select), as.name(dataname), lapply(colnames, as.name)))
}
