testthat::test_that("teal_slice store/restore supports NULL and character(0) for choices and selected", {
  slices_path <- withr::local_file("slices.json")
  tss <- teal_slices(
    teal_slice(
      dataname = "data",
      varname = "var",
      choices = character(0),
      selected = NULL
    )
  )
  slices_store(tss, slices_path)
  tss_restored <- slices_restore(slices_path)

  slices2_path <- withr::local_file("slices2.json")
  tss2 <- teal_slices(
    teal_slice(
      dataname = "data",
      varname = "var",
      choices = NULL,
      selected = NULL
    )
  )
  slices_store(tss2, slices2_path)
  tss2_restored <- slices_restore(slices2_path)

  slices3_path <- withr::local_file("slices3.json")
  tss3 <- teal_slices(
    teal_slice(
      dataname = "data",
      varname = "var",
      choices = character(0),
      selected = character(0)
    )
  )
  slices_store(tss3, slices3_path)
  tss3_restored <- slices_restore(slices3_path)

  teal.slice:::expect_identical_slice(tss[[1]], tss_restored[[1]])
  teal.slice:::expect_identical_slice(tss2[[1]], tss2_restored[[1]])
  teal.slice:::expect_identical_slice(tss3[[1]], tss3_restored[[1]])
})

testthat::test_that("teal_slice store/restore supports saving `POSIXct` timestamps in selected", {
  slices_path <- withr::local_file("slices.json")

  time_stamps <- Sys.time() + c(-10 * 60 * 60 * 24, -30, 0)

  # ISO8601 does not keep milliseconds
  time_stamps <- as.POSIXct(
    ceiling(as.double(time_stamps)),
    tz = "UTC",
    origin = "1970-01-01"
  )

  tss <- teal_slices(
    teal_slice(
      dataname = "ADSL",
      varname = "EOSDTM",
      selected = time_stamps,
      fixed = TRUE
    )
  )

  # Store the teal_slices object to a file
  slices_store(tss, slices_path)
  tss_restored <- slices_restore(slices_path)

  tss_restored_list <- shiny::isolate(shiny::reactiveValuesToList(tss_restored[[1]]))
  testthat::expect_s3_class(tss_restored_list$selected, "POSIXct")

  teal.slice:::expect_identical_slice(tss[[1]], tss_restored[[1]])
})

testthat::test_that("teal_slice store/restore supports saving `Date` dates in selected", {
  slices_path <- withr::local_file("slices.json")

  time_stamps <- Sys.Date() + c(-10 * 600, -30, 0)

  tss <- teal_slices(
    teal_slice(
      dataname = "ADSL",
      varname = "EOSDT",
      selected = time_stamps,
      fixed = TRUE
    )
  )

  # Store the teal_slices object to a file
  slices_store(tss, slices_path)
  tss_restored <- slices_restore(slices_path)

  tss_restored_list <- shiny::isolate(shiny::reactiveValuesToList(tss_restored[[1]]))
  testthat::expect_s3_class(tss_restored_list$selected, "Date")

  teal.slice:::expect_identical_slice(tss[[1]], tss_restored[[1]])
})

testthat::test_that("teal_slice store/restore supports saving `POSIXct` timestamps in choices", {
  slices_path <- withr::local_file("slices.json")

  time_stamps <- Sys.time() + c(-10 * 60 * 60 * 24, -30, 0)

  # ISO8601 does not keep milliseconds
  time_stamps <- as.POSIXct(
    ceiling(as.double(time_stamps)),
    tz = "UTC",
    origin = "1970-01-01"
  )

  tss <- teal_slices(
    teal_slice(
      dataname = "ADSL",
      varname = "EOSDTM",
      selected = sample(time_stamps, 2),
      choices = time_stamps,
      fixed = TRUE
    )
  )

  # Store the teal_slices object to a file
  slices_store(tss, slices_path)
  tss_restored <- slices_restore(slices_path)

  tss_restored_list <- shiny::isolate(shiny::reactiveValuesToList(tss_restored[[1]]))
  testthat::expect_s3_class(tss_restored_list$choices, "POSIXct")

  teal.slice:::expect_identical_slice(tss[[1]], tss_restored[[1]])
})

testthat::test_that("teal_slice store/restore supports saving `Date` timestamps in choices", {
  slices_path <- withr::local_file("slices.json")

  time_stamps <- Sys.Date() + c(-10 * 600, -30, 0)

  tss <- teal_slices(
    teal_slice(
      dataname = "ADSL",
      varname = "EOSDT",
      selected = sample(time_stamps, 2),
      choices = time_stamps,
      fixed = TRUE
    )
  )

  # Store the teal_slices object to a file
  slices_store(tss, slices_path)
  tss_restored <- slices_restore(slices_path)

  tss_restored_list <- shiny::isolate(shiny::reactiveValuesToList(tss_restored[[1]]))
  testthat::expect_s3_class(tss_restored_list$choices, "Date")

  teal.slice:::expect_identical_slice(tss[[1]], tss_restored[[1]])
})


testthat::test_that("teal_slice store/restore restores mixed `Date`-characters as characters in selected", {
  slices_path <- withr::local_file("slices.json")
  tss <- teal_slices(
    teal_slice(
      dataname = "ADSL",
      varname = "EOSDTM",
      selected = c(
        "beta 2023-09-11",
        "release candidate 2023-09-21",
        "release 2023-09-21"
      ),
      fixed = TRUE
    )
  )

  slices_store(tss, slices_path)
  tss_restored <- slices_restore(slices_path)
  teal.slice:::expect_identical_slice(tss[[1]], tss_restored[[1]])
})

testthat::test_that("teal_slice store/restore restores characters as characters in selected and choices", {
  slices_path <- withr::local_file("slices.json")
  tss <- teal_slices(
    teal_slice(
      dataname = "ADSL",
      varname = "EOSDTM",
      choices = c("a", "b", "c"),
      selected = c("a", "b")
    )
  )

  slices_store(tss, slices_path)
  tss_restored <- slices_restore(slices_path)

  testthat::expect_type(shiny::isolate(tss_restored[[1]]$selected), "character")
  testthat::expect_type(shiny::isolate(tss_restored[[1]]$choices), "character")
  teal.slice:::expect_identical_slice(tss[[1]], tss_restored[[1]])
})
