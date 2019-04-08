context("get_code")

file_path  <- "./app_get_code/app.R"
file_path1 <- "./app_get_code/app_source1.R"
file_path2 <- "./app_get_code/app_source2.R"
file_path3 <- "./app_get_code/app_source3.R"

test_that("arguments properly specified", {
  expect_error(get_code(files_path = "non_existing_file.R"))
  expect_error(get_code(files_path = file_path, starts_at = 1))
  expect_error(get_code(files_path = file_path, stops_at = 1))
  expect_error(get_code(files_path = file_path, exclude_comments = "#some comment"))
  expect_error(get_code(files_path = file_path, read_sources = "./app_get_code/app_source1.R"))
})

test_that("Reads code from starts_at to stops_at", {
  code_lines1 <- get_code(files_path = file_path,
                          starts_at = "# @start_code",
                          stops_at = "# @end_code",
                          exclude_comments = FALSE) %>%
    strsplit("\n") %>%
    .[[1]]

  expect_identical(code_lines1[c(1, length(code_lines1))], c("# @start_code", "# @end_code"))


  code_lines2 <- get_code(files_path = file_path,
                          starts_at = "source\\(",
                          stops_at = "teal::init",
                          read_sources = FALSE) %>%
                  strsplit("\n") %>%
                  .[[1]]

  expect_identical(code_lines2[c(1, length(code_lines2))],
                   c("source(\"app_source1.R\")", "x <- teal::init("))

})

test_that("Excludes commented, keep quoted comments", {
  code_lines3 <- get_code(files_path = file_path, exclude_comments = TRUE, read_sources = FALSE) %>%
    strsplit("\n") %>%
    .[[1]]

  excluded <- all(!grepl("(eol comment)|(line comment)|(#nolint)", code_lines3))
  included <- any(grepl("this is not a comment", code_lines3))
  expect_true(excluded & included)
})

test_that("Excludes library calls", {
  code_lines4 <- get_code(files_path = file_path) %>%
    strsplit("\n") %>%
    .[[1]]

  nolibs <- all(!grepl("library\\(", code_lines4))
  expect_true(nolibs)
})


test_that("Excludes #nocode sigle and multi line", {
  code_lines5 <- get_code(files_path = file_path, exclude_comments = FALSE, read_sources = FALSE) %>%
    strsplit("\n") %>%
    .[[1]]

  no_excluded <- any(!grepl("no_code <-", code_lines5))
  expect_true(no_excluded)


  code_lines6 <- get_code(files_path = file_path, exclude_comments = TRUE, read_sources = FALSE) %>%
    strsplit("\n") %>%
    .[[1]]

  no_excluded <- any(!grepl("no_code <-", code_lines6))
  expect_true(no_excluded)


  code_lines7 <- get_code(files_path = file_path, exclude_comments = TRUE, read_sources = TRUE) %>%
    strsplit("\n") %>%
    .[[1]]

  no_excluded <- any(!grepl("no_code <-", code_lines6))
  expect_true(no_excluded)
})


test_that("Include sourced code", {
  code_lines8 <- get_code(files_path = file_path, exclude_comments = TRUE, read_sources = TRUE) %>%
    strsplit("\n") %>%
    .[[1]]

  source1 <- readLines(file_path1, n = 1)
  source2 <- readLines(file_path2, n = 1)
  source3 <- readLines(file_path3, n = 1)

  expect_true(all(c(source1, source2, source3) %in% code_lines8))

})
