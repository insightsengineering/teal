# function definitions ----

# returns list of same shape as `modules`, containing `FilteredData` at every leaf
# if module specific, each leaf contains different instance, otherwise every leaf contains `filtered_data_singleton`
modules_datasets <- function(data, modules, filter, filtered_data_singleton = teal_data_to_filtered_data(data)) {
  checkmate::assert_class(data, "teal_data")
  checkmate::assert_multi_class(modules, c("teal_modules", "teal_module"))
  checkmate::assert_class(filter, "modules_teal_slices")
  checkmate::assert_r6(filtered_data_singleton, "FilteredData")

  if (!isTRUE(attr(filter, "module_specific"))) {
    slices <- shiny::isolate({
      Filter(function(x) x$id %in% attr(filter, "mapping")$global_filters, filter)
    })
    singleton$set_filter_state(slices)
    return(modules_structure(modules, filtered_data_singleton))
  }

  if (inherits(modules, "teal_module")) {
    # 1. get datanames
    datanames <-
      if (is.null(modules$datanames) || identical(modules$datanames, "all")) {
        include_parent_datanames(
          teal_data_datanames(data),
          teal.data::join_keys(data)
        )
      } else {
        modules$datanames
      }
    # 2. subset filters (global + dedicated)
    slices <- shiny::isolate({
      Filter(x = filter, f = function(x) {
        x$dataname %in% datanames &&
          (x$id %in% attr(filter, "mapping")$global_filters ||
            x$id %in% unique(unlist(attr(filter, "mapping")[modules$label])))
      })
    })

    # 3. instantiate FilteredData
    filtered_data <- teal_data_to_filtered_data(data, datanames)
    # 4. set state
    filtered_data$set_filter_state(slices)
    # 5. return
    return(filtered_data)
  } else if (inherits(modules, "teal_modules")) {
    return(lapply(
      modules$children,
      modules_datasets,
      data = data,
      filter = filter,
      filtered_data_singleton = filtered_data_singleton
    ))
  }

  stop("something's not right")
}

# returns nested list of same shape as `modules` with `value` at every leaf
modules_structure <- function(modules, value = TRUE) {
  if (inherits(modules, "teal_module")) {
    return(value)
  } else {
    lapply(modules$children, modules_structure, value)
  }
}


# testing ----
# ## create data ----
# data <- teal_data() %>%
#   within({
#     iris <- iris
#     mtcars <- mtcars
#     women <- women
#   })
# ## create modules ----
# modules <- modules(
#   label = "one",
#   modules(
#     label = "two",
#     example_module("example two", "all"),
#     modules(
#       label = "three",
#       example_module("example three", "iris"),
#       example_module("example four", "mtcars")
#     )
#   ),
#   example_module("example one", "iris")
# )
# ## create filters ----
# filter <- teal_slices(
#   teal_slice("iris", "Species"),
#   teal_slice("iris", "Sepal.Length"),
#   teal_slice("mtcars", "mpg"),
#   teal_slice("mtcars", "cyl"),
#   teal_slice("mtcars", "gear"),
#   module_specific = FALSE,
#   mapping = list(
#     "example one" = "iris Species",
#     "example four" = "mtcars mpg",
#     global_filters = "mtcars cyl"
#   )
# )
#
# # execute and inspect filters ----
# modules_datasets(data, modules, filter) %>%
#   rapply(., function(x) isolate(x$get_filter_state() %>% sapply(`[[`, "id")), how = "replace")
