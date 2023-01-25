testthat::test_that("get_rcode returns header only for empty chunks", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  ch <- teal.code::chunks_new()

  r_code_from_chunks <- strsplit(get_rcode(chunks = ch), "\n")[[1]]
  r_code_from_header <- strsplit(sprintf("\n\n%s\n", paste(get_rcode_header(), collapse = "\n")), "\n")[[1]]

  # removing the Date line from the header as the seconds may be different
  # in the two strings
  testthat::expect_identical(
    r_code_from_chunks[c(1:5, 7:length(r_code_from_chunks))],
    r_code_from_header[c(1:5, 7:length(r_code_from_header))]
  )
})

testthat::test_that("get_rcode returns code from chunks at the end", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  ch <- teal.code::chunks_new()
  teal.code::chunks_push(id = "test", chunks = ch, quote(a <- 1))
  r_code_from_chunks <- strsplit(get_rcode(chunks = ch),  "\n")[[1]]
  testthat::expect_true("a <- 1" %in% r_code_from_chunks)
})

testthat::test_that("get_rcode returns data-loading, filter-panel and chunks code combined", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  ch <- teal.code::chunks_new()
  teal.code::chunks_push(id = "test", chunks = ch, quote(a <- 1))

  datasets <- teal.slice::init_filtered_data(
    teal.data::teal_data(
      teal.data::dataset("IRIS", x = iris, code = "IRIS <- iris"),
      teal.data::dataset("MTCARS", x = mtcars, code = "MTCARS <- mtcars")
    )
  )

  testthat::expect_true(
    all(
      c(
        "IRIS <- iris",
        "MTCARS <- mtcars",
        "a <- 1"
      ) %in%
        strsplit(shiny::isolate(get_rcode(datasets = datasets, chunks = ch)), "\n")[[1]]
    )
  )
})

testthat::test_that("style nested expressions", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  testthat::expect_silent({
    cs <- teal.code::chunks_new()

    cs$push(quote(a <- 1))
    cs$push(quote({
      a <- 1
      b <- 2
    })) # nolint
    cs$push(substitute(c <- 3))
    cs$push(substitute(if (TRUE) d <- 4 else d <- 44))
    # this is special case where default styler failed
    cs$push(substitute({
      if (TRUE) e <- 5 else e <- 55
    })) # nolint
  })

  testthat::expect_silent(
    styler::style_text(cs$get_rcode())
  )
})


testthat::test_that("get_datasets_code returns code only for specified datanames", {
  datasets <- teal.slice::init_filtered_data(
    teal.data::teal_data(
      teal.data::dataset("IRIS", x = iris, code = "IRIS <- iris"),
      teal.data::dataset("MTCARS", x = mtcars, code = "MTCARS <- mtcars")
    )
  )

  hashes <- calculate_hashes(datasets$datanames(), datasets)
  testthat::expect_true(
    !grepl(
      "mtcars",
      paste(get_datasets_code(datasets = datasets, dataname = "IRIS", hashes = hashes), collapse = "\n"),
      ignore.case = TRUE
    ) &&
      grepl(
        "iris",
        paste(get_datasets_code(datasets = datasets, dataname = "IRIS", hashes = hashes), collapse = "\n"),
        ignore.case = TRUE
      )
  )
})
