testthat::test_that("get_teal_bs_theme", {
  testthat::expect_true(is.null(get_teal_bs_theme()))
  withr::with_options(list("teal.bs_theme" = bslib::bs_theme(version = "5")), {
    testthat::expect_s3_class(get_teal_bs_theme(), "bs_theme")
  })
  withr::with_options(list("teal.bs_theme" = 1), {
    testthat::expect_warning(get_teal_bs_theme(), "the default shiny bootstrap is used")
  })
  withr::with_options(list("teal.bs_theme" = "bs_theme"), {
    testthat::expect_warning(get_teal_bs_theme(), "the default shiny bootstrap is used")
  })
})

testthat::test_that("report_card_template function returns TealReportCard object with appropriate content and labels", {
  fd <- teal.slice::init_filtered_data(list(iris = list(dataset = iris)))
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
  datanames(teal_data) <- "iris"

  testthat::expect_s3_class(teal_data_to_filtered_data(teal_data), "FilteredData")
})

test_that("teal_data_datanames returns names of the @env's objects when datanames not set", {
  teal_data <- teal.data::teal_data()
  teal_data <- within(teal_data, {
    iris <- head(iris)
    mtcars <- head(mtcars)
  })
  testthat::expect_setequal(teal_data_datanames(teal_data), c("mtcars", "iris"))
})

test_that("teal_data_datanames returns datanames which are set by teal.data::datanames", {
  teal_data <- teal.data::teal_data()
  teal_data <- within(teal_data, {
    iris <- head(iris)
    mtcars <- head(mtcars)
  })
  datanames(teal_data) <- "iris"
  testthat::expect_equal(teal_data_datanames(teal_data), "iris")
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

  expect_silent(validate_app_title_tag(valid_title))
  expect_error(validate_app_title_tag(head_missing))
  expect_error(validate_app_title_tag(title_missing))
  expect_error(validate_app_title_tag(icon_missing))
})

test_that("build_app_title builts a valid tag", {
  valid_title_local <- build_app_title("title", "logo.png")
  valid_title_remote <- build_app_title("title", "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png") # nolint
  expect_silent(validate_app_title_tag(valid_title_local))
  expect_silent(validate_app_title_tag(valid_title_remote))
})
