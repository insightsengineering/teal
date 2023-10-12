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

testthat::test_that("card_template function returns TealReportCard object with appropriate content and labels", {
  fd <- teal.slice::init_filtered_data(list(iris = list(dataset = iris)))
  filter_panel_api <- teal.slice::FilterPanelAPI$new(fd)

  card <- shiny::isolate(card_template(title = "Card title",
                                       label ="Card label",
                                       description = "Sample description",
                                       with_filter = TRUE,
                                       filter_panel_api =  filter_panel_api))
  testthat::expect_s3_class(card, c("TealReportCard"))
  testthat::expect_equal(card$get_name(), "Card label")
  testthat::expect_length(card$get_content(), 4)

  card <- shiny::isolate(card_template(title = "Card title",
                                       label =  "",
                                       with_filter = FALSE,
                                       filter_panel_api = filter_panel_api))
  testthat::expect_s3_class(card, c("TealReportCard"))
  testthat::expect_equal(card$get_name(), "Card title")
  testthat::expect_length(card$get_content(), 1)
})
