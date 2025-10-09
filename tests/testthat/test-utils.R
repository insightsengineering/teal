testthat::test_that("get_teal_bs_theme", {
  testthat::skip_if_not_installed("bslib")
  testthat::expect_identical(get_teal_bs_theme(), bslib::bs_theme())
  withr::with_options(list("teal.bs_theme" = bslib::bs_theme(version = "5")), {
    testthat::expect_s3_class(get_teal_bs_theme(), "bs_theme")
  })
  withr::with_options(list("teal.bs_theme" = 1), {
    testthat::expect_warning(get_teal_bs_theme(), ".*The default bslib Bootstrap theme will be used.")
  })
  withr::with_options(list("teal.bs_theme" = "bs_theme"), {
    testthat::expect_warning(get_teal_bs_theme(), ".*The default bslib Bootstrap theme will be used.")
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
  testthat::expect_length(card$get_content(), 2)

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
  lifecycle::expect_deprecated(valid_title_local <- build_app_title("title", "logo.png"))
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
    "function1" = function(x) x,
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
    "function1" = "x",
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

# get_unique_labels ----
testthat::test_that("get_unique_labels generates unique labels", {
  labels <- c("Module A", "Module B", "Module A")
  result <- get_unique_labels(labels)
  
  testthat::expect_equal(result, c("module_a", "module_b", "module_a_1"))
  testthat::expect_true(all(!duplicated(result)))
})

testthat::test_that("get_unique_labels converts to lowercase and replaces special chars", {
  labels <- c("Module-A", "Module B!", "Module@C")
  result <- get_unique_labels(labels)
  
  testthat::expect_equal(result, c("module_a", "module_b_", "module_c"))
})

testthat::test_that("get_unique_labels works with empty vector", {
  testthat::expect_equal(get_unique_labels(character(0)), character(0))
})

# pluralize ----
testthat::test_that("pluralize returns singular for length 1", {
  testthat::expect_equal(pluralize(1, "item"), "item")
  testthat::expect_equal(pluralize("x", "item"), "item")
})

testthat::test_that("pluralize returns plural for length > 1", {
  testthat::expect_equal(pluralize(1:5, "item"), "items")
  testthat::expect_equal(pluralize(c("a", "b"), "item"), "items")
})

testthat::test_that("pluralize uses custom plural form when provided", {
  testthat::expect_equal(pluralize(1:5, "child", "children"), "children")
  testthat::expect_equal(pluralize(1, "child", "children"), "child")
})

testthat::test_that("pluralize returns plural for empty vector", {
  testthat::expect_equal(pluralize(character(0), "item"), "items")
  testthat::expect_equal(pluralize(numeric(0), "item"), "items")
})

# .smart_rbind ----
testthat::test_that(".smart_rbind combines data frames with different columns", {
  df1 <- data.frame(a = 1:2, b = 3:4)
  df2 <- data.frame(b = 5:6, c = 7:8)
  
  result <- .smart_rbind(df1, df2)
  
  testthat::expect_equal(nrow(result), 4)
  testthat::expect_equal(ncol(result), 3)
  testthat::expect_true(all(c("a", "b", "c") %in% names(result)))
  testthat::expect_true(is.na(result$c[1]))
  testthat::expect_true(is.na(result$a[3]))
})

testthat::test_that(".smart_rbind works with identical columns", {
  df1 <- data.frame(a = 1:2, b = 3:4)
  df2 <- data.frame(a = 5:6, b = 7:8)
  
  result <- .smart_rbind(df1, df2)
  
  testthat::expect_equal(nrow(result), 4)
  testthat::expect_equal(ncol(result), 2)
})

testthat::test_that(".smart_rbind validates input", {
  testthat::expect_error(
    .smart_rbind(list(), data.frame(a = 1)),
    "Assertion.*failed.*Must inherit from class 'data.frame'"
  )
})

# check_filter_datanames ----
testthat::test_that("check_filter_datanames returns TRUE when all filter datanames are valid", {
  filters <- teal.slice::teal_slices(
    teal.slice::teal_slice(dataname = "iris", varname = "Species")
  )
  
  result <- check_filter_datanames(filters, c("iris", "mtcars"))
  testthat::expect_true(result)
})

testthat::test_that("check_filter_datanames returns error message when filter dataname is invalid", {
  filters <- teal.slice::teal_slices(
    teal.slice::teal_slice(dataname = "missing_data", varname = "x")
  )
  
  result <- check_filter_datanames(filters, c("iris", "mtcars"))
  testthat::expect_match(result, "refers to dataname not available")
  testthat::expect_match(result, "missing_data")
})

# check_modules_datanames ----
testthat::test_that("check_modules_datanames returns TRUE when all module datanames are valid", {
  mod <- module(label = "test", datanames = "iris")
  
  result <- check_modules_datanames(modules(mod), c("iris", "mtcars"))
  testthat::expect_true(result)
})

testthat::test_that("check_modules_datanames returns error message when module dataname is invalid", {
  mod <- module(label = "test", datanames = "missing_data")
  
  result <- check_modules_datanames(modules(mod), c("iris", "mtcars"))
  testthat::expect_match(result, "Dataset")
  testthat::expect_match(result, "missing_data")
})

testthat::test_that("check_modules_datanames accepts 'all' as valid dataname", {
  mod <- module(label = "test", datanames = "all")
  
  result <- check_modules_datanames(modules(mod), c("iris", "mtcars"))
  testthat::expect_true(result)
})

# .include_parent_datanames ----
testthat::test_that(".include_parent_datanames includes parent datasets", {
  join_keys <- teal.data::join_keys(
    teal.data::join_key("parent", "child", c("id" = "parent_id"))
  )
  
  result <- .include_parent_datanames(c("child"), join_keys)
  
  testthat::expect_true("parent" %in% result)
  testthat::expect_true("child" %in% result)
})

testthat::test_that(".include_parent_datanames handles datasets without parents", {
  join_keys <- teal.data::join_keys()
  
  result <- .include_parent_datanames(c("iris"), join_keys)
  
  testthat::expect_equal(result, c("iris"))
})

testthat::test_that(".include_parent_datanames returns unique datanames", {
  join_keys <- teal.data::join_keys(
    teal.data::join_key("parent", "child1", c("id" = "parent_id")),
    teal.data::join_key("parent", "child2", c("id" = "parent_id"))
  )
  
  result <- .include_parent_datanames(c("child1", "child2"), join_keys)
  
  testthat::expect_equal(sum(result == "parent"), 1)
})
