session <- new.env()
session$userData <- new.env() # nolint
session$ns <- function(x) {
  if (length(x) == 0) {
    "id"
  } else {
    paste0("id", x, sep = "-")
  }
} # nolint
y <- c(1, 2)
td <- NULL
dataset <- NULL
teal.code::init_chunks(session = session)
teal.code::chunks_reset(envir = environment(), chunks = session$userData[[session$ns(character(0))]]$chunks)

testthat::test_that("get_rcode for chunks", {
  #- Single chunk with one table construction
  testthat::expect_silent(
    teal.code::chunks_push(
      id = "tbl", expression = rlang::expr(data.frame(x = y)),
      chunks = session$userData[[session$ns(character(0))]]$chunks
    )
  )

  test_data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))

  reference_string <- get_rcode(title = "My title", chunks = session$userData[[session$ns(character(0))]]$chunks)
  compare_string <- tail(strsplit(reference_string, "\n\n")[[1]], 1)

  testthat::expect_equal(
    compare_string,
    "data.frame(x = y)",
    info = "Compare single expression chunk with get_rcode. no data."
  )


  #- Second chunk with replacement function call
  testthat::expect_silent(
    teal.code::chunks_push(
      id = "my_tbl", expression = rlang::expr(summary(td)),
      chunks = session$userData[[session$ns(character(0))]]$chunks
    )
  )

  testthat::expect_error(
    get_rcode(
      title = "My title",
      datasets = list(test_data),
      chunks = session$userData[[session$ns(character(0))]]$chunks
    ),
    "FilteredData",
    info = "get_rcode shall just accept teal.slice::FilteredData"
  )

  reference_string <- get_rcode(title = "My title", chunks = session$userData[[session$ns(character(0))]]$chunks)
  compare_string <- tail(strsplit(reference_string, "\n\n")[[1]], 1)

  testthat::expect_equal(
    compare_string,
    "data.frame(x = y)\nsummary(td)",
    info = "Compare two expression with dataname replacement."
  )


  # Third chunk with if statement which quoted generates errors
  # Note weird break lines in print(quote({if(1){2}else{3}})) # nolint
  testthat::expect_silent(
    teal.code::chunks_push(
      id = "if", expression = quote({
        if (1) {
          2
        } else {
          3
        }
      }), # nolint
      chunks = session$userData[[session$ns(character(0))]]$chunks
    )
  )

  reference_string <- get_rcode(title = "My title", chunks = session$userData[[session$ns(character(0))]]$chunks)
  compare_string <- tail(strsplit(reference_string, "\n\n")[[1]], 1)

  testthat::expect_equal(
    compare_string,
    "data.frame(x = y)\nsummary(td)\nif (1) {\n  2\n} else {\n  3\n}",
    info = "Compare two expression with dataname replacement."
  )

  testthat::expect_silent(
    code_sample <- c(
      paste0(
        "g_spiderplot(marker_x = ..., marker_y = ..., marker_z = ..., funcall(a, b, c = 2, b = ",
        "totalfuncall(zdf = 1, ydf = 3, ydm = 4)), kadsf = asdfk, asdfk = asfk )"
      ),
      "print(abc)"
    )
  )

  testthat::expect_equal(
    as.character(styler::style_text(code_sample)),
    c(
      "g_spiderplot(marker_x = ..., marker_y = ..., marker_z = ..., funcall(a, b, c = 2, b = totalfuncall(zdf = 1, ydf = 3, ydm = 4)), kadsf = asdfk, asdfk = asfk)", # nolint
      "print(abc)"
    )
  )

  session$userData[[session$ns(character(0))]]$chunks <- NULL

  testthat::expect_error(
    get_rcode(chunks = "NULL"),
    "chunks",
    info = "No Code chunks error expected upon empty code chunks."
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
