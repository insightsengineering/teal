testthat::describe("get_bookmarking_option", {
  testthat::it("returns NULL when no bookmarking option is set", {
    old_option <- getOption("shiny.bookmarkStore")
    old_shiny_option <- getShinyOption("bookmarkStore")
    on.exit({
      if (!is.null(old_option)) options(shiny.bookmarkStore = old_option)
      if (!is.null(old_shiny_option)) shinyOptions(bookmarkStore = old_shiny_option)
    })

    options(shiny.bookmarkStore = NULL)
    shinyOptions(bookmarkStore = NULL)

    testthat::expect_null(get_bookmarking_option())
  })

  testthat::it("returns option value when shiny.bookmarkStore is set", {
    old_option <- getOption("shiny.bookmarkStore")
    old_shiny_option <- getShinyOption("bookmarkStore")
    on.exit({
      if (!is.null(old_option)) options(shiny.bookmarkStore = old_option)
      if (!is.null(old_shiny_option)) shinyOptions(bookmarkStore = old_shiny_option)
    })

    options(shiny.bookmarkStore = "server")
    shinyOptions(bookmarkStore = NULL)

    testthat::expect_identical(get_bookmarking_option(), "server")
  })

  testthat::it("returns shiny option value when set", {
    old_option <- getOption("shiny.bookmarkStore")
    old_shiny_option <- getShinyOption("bookmarkStore")
    on.exit({
      if (!is.null(old_option)) options(shiny.bookmarkStore = old_option)
      if (!is.null(old_shiny_option)) shinyOptions(bookmarkStore = old_shiny_option)
    })

    options(shiny.bookmarkStore = NULL)
    shinyOptions(bookmarkStore = "server")

    testthat::expect_identical(get_bookmarking_option(), "server")
  })

  testthat::it("prefers shiny option over R option", {
    old_option <- getOption("shiny.bookmarkStore")
    old_shiny_option <- getShinyOption("bookmarkStore")
    on.exit({
      if (!is.null(old_option)) options(shiny.bookmarkStore = old_option)
      if (!is.null(old_shiny_option)) shinyOptions(bookmarkStore = old_shiny_option)
    })

    options(shiny.bookmarkStore = "url")
    shinyOptions(bookmarkStore = "server")

    testthat::expect_identical(get_bookmarking_option(), "server")
  })
})

testthat::describe("need_bookmarking", {
  testthat::it("returns all FALSE vector when all modules are bookmarkable", {
    modules <- modules(
      module("module_1", server = function(id, data) NULL),
      module("module_2", server = function(id, data) NULL)
    )
    attr(modules$children[[1]], "teal_bookmarkable") <- TRUE
    attr(modules$children[[2]], "teal_bookmarkable") <- TRUE

    result <- need_bookmarking(modules)
    testthat::expect_true(all(!result))
    testthat::expect_length(result, 2L)
  })

  testthat::it("returns vector indicating unbookmarkable modules", {
    modules <- modules(
      module("module_1", server = function(id, data) NULL),
      module("module_2", server = function(id, data) NULL)
    )
    attr(modules$children[[1]], "teal_bookmarkable") <- TRUE
    attr(modules$children[[2]], "teal_bookmarkable") <- FALSE

    result <- need_bookmarking(modules)
    testthat::expect_false(result[1])
    testthat::expect_true(result[2])
    testthat::expect_length(result, 2L)
  })

  testthat::it("returns all TRUE vector when none are bookmarkable", {
    modules <- modules(
      module("module_1", server = function(id, data) NULL),
      module("module_2", server = function(id, data) NULL)
    )
    attr(modules$children[[1]], "teal_bookmarkable") <- FALSE
    attr(modules$children[[2]], "teal_bookmarkable") <- FALSE

    result <- need_bookmarking(modules)
    testthat::expect_true(all(result))
    testthat::expect_length(result, 2L)
  })

  testthat::it("handles nested modules", {
    modules_nested <- modules(
      label = "parent",
      modules(
        label = "nested_group",
        module("nested_1", server = function(id, data) NULL),
        module("nested_2", server = function(id, data) NULL)
      )
    )
    attr(modules_nested$children[[1]]$children[[1]], "teal_bookmarkable") <- TRUE
    attr(modules_nested$children[[1]]$children[[2]], "teal_bookmarkable") <- FALSE

    result <- need_bookmarking(modules_nested)
    testthat::expect_type(result, "logical")
    testthat::expect_length(result, 2L)
    testthat::expect_false(result[1])
    testthat::expect_true(result[2])
  })
})

testthat::describe("srv_bookmark_panel", {
  testthat::it("initializes without error when bookmarking is enabled", {
    old_option <- getOption("shiny.bookmarkStore")
    old_shiny_option <- getShinyOption("bookmarkStore")
    on.exit({
      if (!is.null(old_option)) options(shiny.bookmarkStore = old_option)
      if (!is.null(old_shiny_option)) shinyOptions(bookmarkStore = old_shiny_option)
    })

    options(shiny.bookmarkStore = "server")

    modules <- modules(
      module("module_1", server = function(id, data) NULL)
    )
    attr(modules$children[[1]], "teal_bookmarkable") <- TRUE

    testthat::expect_no_error(
      shiny::testServer(
        app = srv_bookmark_panel,
        args = list(
          id = "test",
          modules = modules
        ),
        expr = NULL
      )
    )
  })

  testthat::it("initializes without error when bookmarking is disabled", {
    old_option <- getOption("shiny.bookmarkStore")
    old_shiny_option <- getShinyOption("bookmarkStore")
    on.exit({
      if (!is.null(old_option)) options(shiny.bookmarkStore = old_option)
      if (!is.null(old_shiny_option)) shinyOptions(bookmarkStore = old_shiny_option)
    })

    options(shiny.bookmarkStore = NULL)

    modules <- modules(
      module("module_1", server = function(id, data) NULL)
    )
    attr(modules$children[[1]], "teal_bookmarkable") <- TRUE

    testthat::expect_no_error(
      shiny::testServer(
        app = srv_bookmark_panel,
        args = list(
          id = "test",
          modules = modules
        ),
        expr = NULL
      )
    )
  })

  testthat::it("does not fail when do_bookmark button is clicked", {
    old_option <- getOption("shiny.bookmarkStore")
    old_shiny_option <- getShinyOption("bookmarkStore")
    on.exit({
      if (!is.null(old_option)) options(shiny.bookmarkStore = old_option)
      if (!is.null(old_shiny_option)) shinyOptions(bookmarkStore = old_shiny_option)
    })

    options(shiny.bookmarkStore = "server")

    modules <- modules(
      module("module_1", server = function(id, data) NULL)
    )
    attr(modules$children[[1]], "teal_bookmarkable") <- TRUE

    shiny::testServer(
      app = srv_bookmark_panel,
      args = list(
        id = "test",
        modules = modules
      ),
      expr = {
        testthat::expect_no_error({
          session$setInputs(do_bookmark = 1)
        })
      }
    )
  })
})
