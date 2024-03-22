testthat::test_that("e2e: Create empty snapshot", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    timeout = 20 * 1000
  )

  app$open_snapshot_manager()

  ns <- shiny::NS(app$wunder_bar_ns("snapshot_manager"))

  app$click(ns("snapshot_add"))
  app$wait_for_idle()

  app$set_input(ns("snapshot_name"), "Empty_Snapshot")

  app$click(ns("snapshot_name_accept"))
  app$wait_for_idle()

  testthat::expect_equal(
    app$get_text(selector = sprintf("#%s .manager_table_row span h5", ns("snapshot_list"))),
    "Empty_Snapshot"
  )
  app$stop()
})

testthat::test_that("e2e: Downloads empty snapshot", {
  skip_if_not_installed("withr")

  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    timeout = 20 * 1000
  )

  app$open_snapshot_manager()

  ns <- shiny::NS(app$wunder_bar_ns("snapshot_manager"))

  app$click(ns("snapshot_add"))
  app$wait_for_idle()

  app$set_input(ns("snapshot_name"), "Empty_Snapshot")

  app$click(ns("snapshot_name_accept"))
  app$wait_for_idle()

  # Path for downloaded file
  local_snapshot <- withr::local_tempfile(fileext = ".json")
  app$get_download(
    output = ns("saveme_Empty_Snapshot"),
    filename = local_snapshot
  )

  tss <- slices_restore(local_snapshot)
  testthat::expect_length(tss, 0)

  app$stop()
})

testthat::test_that("e2e: Download filter snapshot with non-empty filters", {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "Example Module"),
    timeout = 20 * 1000
  )

  app$add_filter_var("iris", "Species")
  app$set_active_filter_selection("iris", "Species", c("setosa", "virginica"))

  app$open_snapshot_manager()

  ns <- shiny::NS(app$wunder_bar_ns("snapshot_manager"))

  app$click(ns("snapshot_add"))
  app$wait_for_idle()

  app$set_input(ns("snapshot_name"), "A_Snapshot")

  app$click(ns("snapshot_name_accept"))
  app$wait_for_idle()

  # Path for downloaded file
  local_snapshot <- withr::local_tempfile(fileext = ".json")
  app$get_download(output = ns("saveme_A_Snapshot"), filename = local_snapshot)

  tss <- slices_restore(local_snapshot)
  testthat::expect_setequal(shiny::isolate(tss[[1]]$selected), c("setosa", "virginica"))

  app$stop()
})

testthat::test_that("e2e: Upload filter snapshot with non-empty filters", {
  skip_if_not_installed("withr")

  data <- simple_teal_data()
  mods <- example_module(label = "module1")

  app <- TealAppDriver$new(
    data = data,
    modules = mods,
    timeout = 20 * 1000
  )

  # Build and save to file a valid teal_slice
  tss <- teal_slices(
    teal_slice(dataname = "iris", varname = "Species", id = "species"),
    teal_slice(dataname = "iris", varname = "Sepal.Length", id = "sepal_length"),
    teal_slice(
      dataname = "iris", id = "long_petals", title = "Long petals", expr = "Petal.Length > 5"
    ),
    teal_slice(dataname = "mtcars", varname = "mpg", id = "mtcars_mpg"),
    app_id = create_app_id(data, modules(mods))
  )

  # Slices need to be in a file on the current directory as it doesn't accept
  # absolute path despite the documentation:
  # https://rstudio.github.io/shinytest2/articles/in-depth.html?q=file_upload#uploading-files
  local_snapshot <- withr::local_file("temp_slices.json")
  slices_store(tss, local_snapshot)

  app$open_snapshot_manager()

  ns <- shiny::NS(app$wunder_bar_ns("snapshot_manager"))

  app$click(ns("snapshot_load"))
  app$wait_for_idle()

  app$upload_file(!!ns("snapshot_file") := local_snapshot)

  app$click(ns("snaphot_file_accept"))
  app$wait_for_idle()

  testthat::expect_setequal(
    names(app$get_active_data_filters("iris")),
    c("Species", "Sepal.Length", "long_petals")
  )

  app$stop()
})

testthat::test_that("e2e: Snapshot manager can reset the state", {
  skip_if_not_installed("withr")

  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = example_module(label = "module1"),
    timeout = 20 * 1000
  )

  app$add_filter_var("iris", "Species")
  app$set_active_filter_selection("iris", "Species", c("setosa", "virginica"))

  app$open_snapshot_manager()

  ns <- shiny::NS(app$wunder_bar_ns("snapshot_manager"))

  app$click(ns("snapshot_reset"))
  app$wait_for_idle(500)

  testthat::expect_length(app$get_active_data_filters("iris"), 0)
  testthat::expect_length(app$get_active_data_filters("mtcars"), 0)

  app$stop()
})
