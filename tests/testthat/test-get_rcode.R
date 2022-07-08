testthat::test_that("get_rcode returns header only for empty chunks", {
  ch <- teal.code::chunks_new()

  r_code_from_chunks <- get_rcode(chunks = ch)
  r_code_from_header <- sprintf("\n\n%s\n", paste(get_rcode_header(), collapse = "\n"))

  # removing the Date line from the header as the seconds may be different
  # in the two strings
  testthat::expect_identical(
    strsplit(r_code_from_chunks, "\n")[[1]][c(1:5, 7:14)],
    strsplit(r_code_from_header, "\n")[[1]][c(1:5, 7:14)]
  )
})

testthat::test_that("get_rcode returns code from chunks at the end", {
  ch <- teal.code::chunks_new()
  teal.code::chunks_push(id = "test", chunks = ch, quote(a <- 1))
  header <- get_rcode_header()
  print("header:")
  print(header)
  exp <- sprintf("\n\n%s\n\na <- 1", paste(header, collapse = "\n"))
  act <- get_rcode(chunks = ch)  
  print("exp:")
  print(exp)
  print(str(exp))
  print("act:")
  print(act)
  print(str(act))
  testthat::expect_identical(
    exp,
    act
  )
})

testthat::test_that("get_rcode returns data-loading, filter-panel and chunks code combined", {
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
        strsplit(get_rcode(datasets = datasets, chunks = ch), "\n")[[1]]
    )
  )
})

testthat::test_that("style nested expressions", {
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

  testthat::expect_true(
    !grepl(
      "mtcars",
      paste(get_datasets_code(datasets = datasets, dataname = "IRIS"), collapse = "\n"),
      ignore.case = TRUE
    ) &&
      grepl(
        "iris",
        paste(get_datasets_code(datasets = datasets, dataname = "IRIS"), collapse = "\n"),
        ignore.case = TRUE
      )
  )
})
