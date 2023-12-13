testthat::test_that("invalid arguments raise errors", {
  testthat::expect_error(
    validate_inputs("string"),
    "validate_inputs accepts validators or a list thereof"
  )
  testthat::expect_error(
    validate_inputs(list("name" = "string")),
    "validate_inputs accepts validators or a list thereof"
  )
})


testthat::test_that("disabled validators raise warnings (individual validators)", {
  server <- function(input, output, session) {
    iv1 <- shinyvalidate::InputValidator$new()
    iv1$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv1$enable()
    iv2 <- shinyvalidate::InputValidator$new()
    iv2$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    values <- shiny::reactive({
      validate_inputs(iv1, iv2)
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]]
      )
    })
  }

  shiny::testServer(server, {
    testthat::expect_warning(
      object = values(),
      regexp = "Validator is disabled and will be omitted."
    )
  })
})


testthat::test_that("disabled validators raise warnings (validator list)", {
  server <- function(input, output, session) {
    iv1 <- shinyvalidate::InputValidator$new()
    iv1$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv1$enable()
    iv2 <- shinyvalidate::InputValidator$new()
    iv2$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    values <- shiny::reactive({
      validate_inputs(list(iv1, iv2))
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]]
      )
    })
  }

  shiny::testServer(server, {
    testthat::expect_warning(
      object = values(),
      regexp = "Validator is disabled and will be omitted."
    )
  })
})


testthat::test_that("disabled validators raise warnings (nested validator list)", {
  server <- function(input, output, session) {
    iv1 <- shinyvalidate::InputValidator$new()
    iv1$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv1$enable()
    iv2 <- shinyvalidate::InputValidator$new()
    iv2$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    values <- shiny::reactive({
      validate_inputs(list(list(iv1), list(iv2)))
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]]
      )
    })
  }

  shiny::testServer(server, {
    testthat::expect_warning(
      object = values(),
      regexp = "Validator is disabled and will be omitted."
    )
  })
})


testthat::test_that("valid inputs produce desired output (individual validators)", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      validate_inputs(iv)
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


testthat::test_that("valid inputs produce desired output (validator list)", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      validate_inputs(list(iv))
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


testthat::test_that("valid inputs produce desired output (nested validator list)", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      validate_inputs(list(list(iv)))
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


testthat::test_that("invalid inputs raise errors in output (individual validators)", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      validate_inputs(iv)
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
    testthat::expect_error(values(), "choose a capital letter")
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "A",
      "number" = 1L
    )
    testthat::expect_error(values(), "choose an even number")
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "a",
      "number" = 1L
    )
    testthat::expect_error(values(), "choose a capital letter.+choose an even number")
  })
})


testthat::test_that("invalid inputs raise errors in output (validator list)", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      validate_inputs(list(iv))
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
    testthat::expect_error(values(), "choose a capital letter")
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "A",
      "number" = 1L
    )
    testthat::expect_error(values(), "choose an even number")
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "a",
      "number" = 1L
    )
    testthat::expect_error(values(), "choose a capital letter.+choose an even number")
  })
})

testthat::test_that("invalid inputs raise errors in output (nested validator list)", {
  server <- function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("letter", shinyvalidate::sv_in_set(LETTERS, "choose a capital letter"))
    iv$add_rule("number", ~ if (as.integer(.) %% 2L == 1L) "choose an even number")
    iv$enable()
    values <- shiny::reactive({
      validate_inputs(list(list(iv)))
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
    testthat::expect_error(values(), "choose a capital letter")
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "A",
      "number" = 1L
    )
    testthat::expect_error(values(), "choose an even number")
  })
  shiny::testServer(server, {
    session$setInputs(
      "letter" = "a",
      "number" = 1L
    )
    testthat::expect_error(values(), "choose a capital letter.+choose an even number")
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
    testthat::expect_error(validate_inputs(iv), class = "shiny.silent.error")
    testthat::expect_error(validate_inputs(iv, iv_par), class = "shiny.silent.error")
    testthat::expect_error(validate_inputs(list(iv)), class = "shiny.silent.error")
    testthat::expect_error(validate_inputs(list(iv, iv_par)), class = "shiny.silent.error")
    testthat::expect_error(validate_inputs(list(iv, list(iv_par))), class = "shiny.silent.error")

    # check error message
    errmess <- tryCatch(validate_inputs(iv), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Some inputs require attention\n",
        "choose a capital letter",
        "choose an even number",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(validate_inputs(iv, iv_par), error = function(e) e$message)
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

    errmess <- tryCatch(validate_inputs(list(iv)), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Some inputs require attention\n",
        "choose a capital letter",
        "choose an even number",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(validate_inputs(list(iv, iv_par)), error = function(e) e$message)
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

    errmess <- tryCatch(validate_inputs(list(iv, list(iv_par))), error = function(e) e$message)
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

    # check custom headers
    errmess <- tryCatch(validate_inputs(iv, header = "Header message"), error = function(e) e$message)
    testthat::expect_identical(errmess, paste(
      c(
        "Header message\n",
        "choose a capital letter",
        "choose an even number",
        "\n"
      ),
      collapse = "\n"
    ))

    errmess <- tryCatch(validate_inputs(iv, iv_par, header = "Header message"), error = function(e) e$message)
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

    errmess <- tryCatch(validate_inputs(list("Header message" = iv)), error = function(e) e$message)
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
      validate_inputs(list(
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

    errmess <- tryCatch(
      validate_inputs(list(
        "Header message 1" = iv,
        list(
          "Header message 2" = iv_par
        )
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

    errmess <- tryCatch(
      validate_inputs(list(
        iv,
        "Header message" = iv_par
      )),
      error = function(e) e$message
    )
    testthat::expect_identical(errmess, paste(
      c(
        "choose a capital letter",
        "choose an even number",
        "Header message\n",
        "choose a color",
        "choose a value between 0.5 and 3",
        "\n"
      ),
      collapse = "\n"
    ))
  })
})


testthat::test_that("different validation modes produce proper messages", {
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
      validate_inputs(iv, header = "Main validator")
      validate_inputs(iv_par, header = "Graphical validator")
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]],
        "color" = input[["color"]],
        "size" = input[["size"]]
      )
    })
    values_c <- shiny::reactive({
      validate_inputs(iv, iv_par, header = "Both validators")
      list(
        "letter" = input[["letter"]],
        "number" = input[["number"]],
        "color" = input[["color"]],
        "size" = input[["size"]]
      )
    })
    values_g <- shiny::reactive({
      validate_inputs(list("Main validator" = iv, "Graphical validator" = iv_par))
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
