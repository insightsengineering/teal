file_path <- "./app_get_code/app.R"
file_path1 <- "./app_get_code/app_source1.R"
file_path2 <- "./app_get_code/app_source2.R"
file_path3 <- "./app_get_code/app_source3.R"

test_that("arguments properly specified", {
  expect_error(get_code(files_path = "non_existing_file.R"))
  expect_error(get_code(files_path = file_path, exclude_comments = "#some comment"))
  expect_error(get_code(files_path = file_path, read_sources = "./app_get_code/app_source1.R"))
})

test_that("Reads code from starts_at to stops_at", {
  code_lines1 <- get_code(files_path = file_path, exclude_comments = FALSE) %>%
    strsplit("\n") %>%
    .[[1]]

  expect_identical(code_lines1[c(length(code_lines1))], c("# \"this is a comment\""))
})

test_that("Excludes commented, keep quoted comments", {
  code_lines3 <- get_code(files_path = file_path, exclude_comments = TRUE, read_sources = FALSE) %>%
    strsplit("\n") %>%
    .[[1]]

  excluded <- all(!grepl("(eol comment)|(line comment)|(#nolint)", code_lines3))
  included <- any(grepl("this is not a comment", code_lines3))
  expect_true(excluded & included)
})


test_that("Excludes #nocode sigle and multi line", {
  code_lines5 <- get_code(files_path = file_path, exclude_comments = FALSE, read_sources = FALSE) %>%
    strsplit("\n") %>%
    .[[1]]

  no_excluded <- any(!grepl("(nocode\\s*>)|(<\\s*nocode)|(nocode <-)|(#\\s*nocode)", code_lines5))
  expect_true(no_excluded)


  code_lines6 <- get_code(files_path = file_path, exclude_comments = FALSE, read_sources = TRUE) %>%
    strsplit("\n") %>%
    .[[1]]

  no_excluded <- any(!grepl("(nocode\\s*>)|(<\\s*nocode)|(nocode <-)|(#\\s*nocode)", code_lines6))
  expect_true(no_excluded)


  code_lines7 <- get_code(files_path = file_path, exclude_comments = TRUE, read_sources = TRUE) %>%
    strsplit("\n") %>%
    .[[1]]

  no_excluded <- any(!grepl("(nocode\\s*>)|(<\\s*nocode)|(nocode <-)|(#\\s*nocode)", code_lines6))
  expect_true(no_excluded)
})


test_that("Include sourced code", {
  code_lines8 <- get_code(files_path = file_path, exclude_comments = FALSE, read_sources = TRUE) %>%
    strsplit("\n") %>%
    .[[1]]

  source1 <- readLines(file_path1, n = 1)
  source2 <- readLines(file_path2, n = 1)
  source3 <- readLines(file_path3, n = 1)

  expect_true(all(c(source1, source2, source3) %in% code_lines8))
})

testthat::test_that("get_code.TealDataset returns identical code to TealDataset$get_code", {
  test_ds1 <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")

  testthat::expect_identical(get_code(test_ds1, deparse = TRUE), test_ds1$get_code(deparse = TRUE))
  testthat::expect_identical(get_code(test_ds1, deparse = FALSE), test_ds1$get_code(deparse = FALSE))

  testthat::expect_warning(get_code(test_ds1, unused_arg = FALSE))
})

testthat::test_that("get_code.TealDatasetConnector returns identical code to TealDatasetConnector$get_code", {
  test_ds1 <- TealDataset$new("head_mtcars", head(mtcars), code = "head_mtcars <- head(mtcars)")

  pull_fun <- callable_function(data.frame)
  t_dc <- dataset_connector("test_dc", pull_fun, vars = list(test_ds1 = test_ds1))

  testthat::expect_identical(get_code(t_dc, deparse = TRUE), t_dc$get_code(deparse = TRUE))
  testthat::expect_identical(get_code(t_dc, deparse = FALSE), t_dc$get_code(deparse = FALSE))

  testthat::expect_warning(get_code(t_dc, unused_arg = FALSE))
})

testthat::test_that("get_code.TealDataAbstract returns identical code to TealDataAbstract$get_code", {
  x1 <- dataset(
    x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
    keys = "y",
    dataname = "XY",
    code = "XY <- data.frame(x = c(1, 2), y = c('aa', 'bb'), stringsAsFactors = FALSE)",
    label = character(0)
  )

  x2 <- dataset(
    x = data.frame(x = c(1, 2), y = c("a", "b"), stringsAsFactors = FALSE),
    keys = "y",
    dataname = "XYZ",
    code = "XYZ <- data.frame(x = c(1, 2), y = c('aa', 'bb'), stringsAsFactors = FALSE)",
    label = character(0)
  )

  rd <- teal_data(x1, x2)

  testthat::expect_identical(get_code(rd, deparse = TRUE), rd$get_code(deparse = TRUE))
  testthat::expect_identical(get_code(rd, deparse = FALSE), rd$get_code(deparse = FALSE))

  testthat::expect_identical(
    get_code(rd, dataname = "XY", deparse = TRUE),
    rd$get_code(dataname = "XY", deparse = TRUE)
  )
  testthat::expect_identical(
    get_code(rd, dataname = "XY", deparse = FALSE),
    rd$get_code(dataname = "XY", deparse = FALSE)
  )

  testthat::expect_identical(
    get_code(rd, dataname = "XYZ", deparse = TRUE),
    rd$get_code(dataname = "XYZ", deparse = TRUE)
  )
  testthat::expect_identical(
    get_code(rd, dataname = "XYZ", deparse = FALSE),
    rd$get_code(dataname = "XYZ", deparse = FALSE)
  )

  testthat::expect_warning(get_code(rd, unused_arg = FALSE))
})
