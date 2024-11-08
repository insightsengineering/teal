testthat::test_that("get_teal_bs_theme", {
  testthat::skip_if_not_installed("bslib")
  testthat::expect_true(is.null(get_teal_bs_theme()))
  withr::with_options(list("teal.bs_theme" = bslib::bs_theme(version = "5")), {
    testthat::expect_s3_class(get_teal_bs_theme(), "bs_theme")
  })
  withr::with_options(list("teal.bs_theme" = 1), {
    testthat::expect_warning(get_teal_bs_theme(), ".*The default Shiny Bootstrap theme will be used.")
  })
  withr::with_options(list("teal.bs_theme" = "bs_theme"), {
    testthat::expect_warning(get_teal_bs_theme(), ".*The default Shiny Bootstrap theme will be used.")
  })
})

testthat::test_that("report_card_template function returns TealReportCard object with appropriate content and labels", {
  fd <- teal.slice::init_filtered_data(list(iris = iris))
  filter_panel_api <- teal.slice::FilterPanelAPI$new(fd)

  card <- shiny::isolate(report_card_template(
    title = "Card title",
    label = "Card label",
    description = "Sample description",
    with_filter = TRUE,
    filter_panel_api = filter_panel_api
  ))
  testthat::expect_s3_class(card, c("TealReportCard"))
  testthat::expect_equal(card$get_name(), "Card label")
  testthat::expect_length(card$get_content(), 4)

  card <- shiny::isolate(report_card_template(
    title = "Card title",
    label = "",
    with_filter = FALSE,
    filter_panel_api = filter_panel_api
  ))
  testthat::expect_s3_class(card, c("TealReportCard"))
  testthat::expect_equal(card$get_name(), "Card title")
  testthat::expect_length(card$get_content(), 1)
})

test_that("teal_data_to_filtered_data return FilteredData class", {
  teal_data <- teal.data::teal_data()
  teal_data <- within(teal_data, iris <- head(iris))

  testthat::expect_s3_class(teal_data_to_filtered_data(teal_data), "FilteredData")
})

test_that("validate_app_title_tag works on validating the title tag", {
  valid_title <- tags$head(
    tags$title("title"),
    tags$link(rel = "icon", href = "favicon.ico"),
    tags$div("Secret")
  )

  head_missing <- tags$div(
    tags$title(title),
    tags$link(rel = "icon", href = "favicon.ico")
  )
  title_missing <- tags$head(
    tags$link(rel = "icon", href = "favicon.ico")
  )
  icon_missing <- tags$head(
    tags$title(title)
  )
  invalid_link <- tags$head(
    tags$title("title"),
    tags$link(href = "favicon.ico"),
    tags$div("Secret")
  )

  testthat::expect_silent(validate_app_title_tag(valid_title))
  testthat::expect_error(validate_app_title_tag(head_missing))
  testthat::expect_error(validate_app_title_tag(title_missing))
  testthat::expect_error(validate_app_title_tag(icon_missing))
  testthat::expect_error(validate_app_title_tag(invalid_link))
})

test_that("build_app_title builts a valid tag", {
  valid_title_local <- build_app_title("title", "logo.png")
  valid_title_remote <- build_app_title("title", "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png") # nolint
  testthat::expect_silent(validate_app_title_tag(valid_title_local))
  testthat::expect_silent(validate_app_title_tag(valid_title_remote))
})


# create_app_id ----
testthat::test_that("create_app_id: 'data' accepts teal_data or teal_data_module", {
  testthat::expect_no_error(create_app_id(teal.data::teal_data(), modules(example_module())))

  tdm <- teal_data_module(
    ui = function(id) tags$div(),
    server = function(id) NULL
  )
  testthat::expect_no_error(create_app_id(tdm, modules(example_module())))

  testthat::expect_error(
    create_app_id(iris, modules(example_module())),
    "Assertion on 'data' failed: Must inherit from class 'teal_data'/'teal_data_module'"
  )
})

testthat::test_that("create_app_id: 'modules' accepts modules", {
  testthat::expect_no_error(create_app_id(teal.data::teal_data(), modules(example_module())))

  testthat::expect_error(
    create_app_id(teal.data::teal_data(), example_module()),
    "Assertion on 'modules' failed: Must inherit from class 'teal_modules'"
  )
})

testthat::test_that("create_app_id returns a character string", {
  checkmate::expect_string(create_app_id(teal.data::teal_data(), modules(example_module())))
})

testthat::test_that("create_app_id returns different hash for different data", {
  hash1 <- create_app_id(teal.data::teal_data(i = iris), modules(example_module()))
  hash2 <- create_app_id(teal.data::teal_data(i = mtcars), modules(example_module()))
  testthat::expect_failure(testthat::expect_identical(hash1, hash2))
})

testthat::test_that("create_app_id returns different hash for different modules", {
  hash1 <- create_app_id(teal.data::teal_data(i = iris), modules(example_module()))
  hash2 <- create_app_id(teal.data::teal_data(i = iris), modules(example_module(), example_module()))
  testthat::expect_failure(testthat::expect_identical(hash1, hash2))
})

## defunction ----
testthat::test_that("defunction returns a string when passed a function", {
  checkmate::expect_string(defunction(init))
})

testthat::test_that("defunction returns non-function atomic as is", {
  testthat::expect_identical(
    defunction("character"),
    "character"
  )
  testthat::expect_identical(
    defunction(c(TRUE, FALSE)),
    c(TRUE, FALSE)
  )
  testthat::expect_identical(
    defunction(1:3),
    1:3
  )
  testthat::expect_identical(
    defunction(1:3 * 1),
    1:3 * 1
  )
})

testthat::test_that("defunction recursively goes down a list", {
  # styler: off
  x <- list(
    "character" = "character",
    "function1" = function(x) return(x),
    "list2" = list(
      "function2" = function(x) mean(x),
      "list3" = list(
        "function3" = function(data) summary(data)
      )
    )
  )
  # styler: on
  y <- list(
    "character" = "character",
    "function1" = "return(x)",
    "list2" = list(
      "function2" = "mean(x)",
      "list3" = list(
        "function3" = "summary(data)"
      )
    )
  )
  testthat::expect_identical(
    defunction(x),
    y
  )
})
