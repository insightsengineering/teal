
testthat::test_that("invalid arguments raise errors", {
  testthat::expect_error(gather_files("string"))
  testthat::expect_error(gather_files_com("string"))
  testthat::expect_error(gather_files_grp(list("name" = "string")))
})


testthat::test_that("gather_fails: valid inputs produce desired output", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      gather_fails(iv)
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]]
      )
    })
  }

  shiny::testServer(server, {
    session$setInputs(
      "letter" = "A",
      "number" = 2L
    )
    testthat::expect_identical(values(), list(
      "letter" = input[["letter"]],
      "number" = input[["number"]]
    ))
  })
})

testthat::test_that("gather_fails_com: valid inputs produce desired output", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      gather_fails_com(iv)
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]]
      )
    })
  }

  shiny::testServer(server, {
    session$setInputs(
      "letter" = "A",
      "number" = 2L
    )
    testthat::expect_identical(values(), list(
      "letter" = input[["letter"]],
      "number" = input[["number"]]
    ))
  })
})

testthat::test_that("gather_fails_grp: valid inputs produce desired output", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      gather_fails_grp(list(iv))
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]]
      )
    })
  }

  shiny::testServer(server, {
    session$setInputs(
      "letter" = "A",
      "number" = 2L
    )
    testthat::expect_identical(values(), list(
      "letter" = input[["letter"]],
      "number" = input[["number"]]
    ))
  })
})


testthat::test_that("gather_fails: invalid inputs raise errors in output", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      gather_fails(iv)
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]]
      )
    })
  }

  shiny::testServer(server, {
    session$setInputs(
      "letter" = "a",
      "number" = 2L
    )
    testthat::expect_error(values())
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "A",
      "number" = 1L
    )
    testthat::expect_error(values())
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "a",
      "number" = 1L
    )
    testthat::expect_error(values())
  })
})

testthat::test_that("gather_fails_com: invalid inputs raise errors in output", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      gather_fails_com(iv)
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]]
      )
    })
  }

  shiny::testServer(server, {
    session$setInputs(
      "letter" = "a",
      "number" = 2L
    )
    testthat::expect_error(values())
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "A",
      "number" = 1L
    )
    testthat::expect_error(values())
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "a",
      "number" = 1L
    )
    testthat::expect_error(values())
  })
})

testthat::test_that("gather_fails_grp: invalid inputs raise errors in output", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      gather_fails_grp(list(iv))
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]]
      )
    })
  }

  shiny::testServer(server, {
    session$setInputs(
      "letter" = "a",
      "number" = 2L
    )
    testthat::expect_error(values())
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "A",
      "number" = 1L
    )
    testthat::expect_error(values())
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "a",
      "number" = 1L
    )
    testthat::expect_error(values())
  })
})


testthat::test_that("error message is formatted properly", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    iv_par <- shinyvalidate::InputValidator$new()
    iv_par$add_rule("color", shinyvalidate::sv_required(message = "choose a color"))
    iv_par$add_rule(
      "size",
      shinyvalidate::sv_between(
        left = 0.5, right = 3,
        message_fmt = "choose a value between {left} and {right}"
      )
    )
    iv_par$enable()
  }

  shiny::testServer(server, {
    session$setInputs(
      "letter" = "a",
      "number" = 1L,
      "color" = "",
      "size" = 0.25
    )
    # check error class
    testthat::expect_error(gather_fails(iv), class = "shiny.silent.error")
    testthat::expect_error(gather_fails_com(iv), class = "shiny.silent.error")
    testthat::expect_error(gather_fails_com(iv, iv_par), class = "shiny.silent.error")
    testthat::expect_error(gather_fails_grp(list(iv)), class = "shiny.silent.error")
    testthat::expect_error(gather_fails_grp(list(iv, iv_par)), class = "shiny.silent.error")

    # check error message
    errmess <- tryCatch(gather_fails(iv), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Some inputs require attention\n",
        "choose a capital letter",
        "choose an even number",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(gather_fails_com(iv), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Some inputs require attention\n",
        "choose a capital letter",
        "choose an even number",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(gather_fails_com(iv, iv_par), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Some inputs require attention\n",
        "choose a capital letter",
        "choose an even number",
        "choose a color",
        "choose a value between 0.5 and 3",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(gather_fails_grp(list(iv)), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "\n",
        "choose a capital letter",
        "choose an even number",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(gather_fails_grp(list(iv, iv_par)), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "\n",
        "choose a capital letter",
        "choose an even number",
        "\n",
        "\n",
        "choose a color",
        "choose a value between 0.5 and 3",
        "\n"
      ),
      collapse = "\n"
    ))

    # check custom headers
    errmess <- tryCatch(gather_fails(iv, "Header message"), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Header message\n",
        "choose a capital letter",
        "choose an even number",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(gather_fails_com(iv, header = "Header message"), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Header message\n",
        "choose a capital letter",
        "choose an even number",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(gather_fails_com(iv, iv_par, header = "Header message"), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Header message\n",
        "choose a capital letter",
        "choose an even number",
        "choose a color",
        "choose a value between 0.5 and 3",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(gather_fails_grp(list("Header message" = iv)), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Header message\n",
        "choose a capital letter",
        "choose an even number",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(
      gather_fails_grp(list(
        "Header message 1" = iv,
        "Header message 2" = iv_par
      )),
      error = function(e) e$message
    )
    testthat::expect_identical(errmess, paste(
      c(
        "Header message 1\n",
        "choose a capital letter",
        "choose an even number",
        "\n",
        "Header message 2\n",
        "choose a color",
        "choose a value between 0.5 and 3",
        "\n"
      ),
      collapse = "\n"
    ))
  })
})


testthat::test_that("hierarchical validation", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    iv_par <- shinyvalidate::InputValidator$new()
    iv_par$add_rule("color", shinyvalidate::sv_required(message = "choose a color"))
    iv_par$add_rule(
      "size",
      shinyvalidate::sv_between(
        left = 0.5, right = 3,
        message_fmt = "choose a value between {left} and {right}"
      )
    )
    iv_par$enable()

    values_h <- shiny::reactive({
      gather_fails(iv, header = "Main validator")
      gather_fails(iv_par, header = "Graphical validator")
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]],
        "color" = input[["color"]],
        "size" = input[["size"]]
      )
    })
    values_c <- shiny::reactive({
      gather_fails_com(iv, iv_par, header = "Both validators")
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]],
        "color" = input[["color"]],
        "size" = input[["size"]]
      )
    })
    values_g <- shiny::reactive({
      gather_fails_grp(list("Main validator" = iv, "Graphical validator" = iv_par))
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]],
        "color" = input[["color"]],
        "size" = input[["size"]]
      )
    })
  }

  shiny::testServer(server, {
    session$setInputs(
      "method" = "hierarchical",
      "letter" = "a",
      "number" = 1L,
      "color" = "",
      "size" = 0.25
    )

    errmess <- tryCatch(values_h(), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Main validator\n",
        "choose a capital letter",
        "choose an even number",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(values_c(), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Both validators\n",
        "choose a capital letter",
        "choose an even number",
        "choose a color",
        "choose a value between 0.5 and 3",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(values_g(), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Main validator\n",
        "choose a capital letter",
        "choose an even number",
        "\n",
        "Graphical validator\n",
        "choose a color",
        "choose a value between 0.5 and 3",
        "\n"
      ),
      collapse = "\n"
    ))
  })
})
