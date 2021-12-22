file_path <- "./app_get_code_dataname/app.R"
file_path1 <- "./app_get_code_dataname/app_source1.R"
file_path2 <- "./app_get_code_dataname/app_source2.R"
file_path3 <- "./app_get_code_dataname/app_source3.R"
file_path_nochunks <- "./app_get_code_dataname/app_nochunks.R"

test_that("arguments properly specified", {
  expect_error(get_code(files_path = "non_existing_file.R"))
  expect_error(get_code(files_path = file_path, exclude_comments = "#some comment"))
  expect_error(get_code(files_path = file_path, dataname = "ADSL", exclude_comments = "#some comment"))
  expect_error(get_code(
    files_path = file_path, dataname = "ADSL",
    read_sources = "./app_get_code/app_source1.R"
  ))
})

test_that("Reads code from starts_at to stops_at", {
  code_lines1 <- get_code(files_path = file_path, dataname = "ADSL", exclude_comments = FALSE) %>%
    strsplit("\n") %>%
    .[[1]]

  expect_identical(code_lines1[c(length(code_lines1))], c("# \"this is a comment\""))
})

test_that("Excludes commented, keep quoted comments", {
  code_lines3 <- get_code(
    files_path = file_path, dataname = "ADSL", exclude_comments = TRUE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]

  excluded <- all(!grepl("(eol comment)|(line comment)|(#nolint)", code_lines3))
  included <- any(grepl("this is not a comment", code_lines3))
  expect_true(excluded & included)
})


test_that("Excludes #nocode sigle and multi line", {
  code_lines5 <- get_code(
    files_path = file_path, dataname = "ADSL", exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]

  no_excluded <- any(!grepl("(nocode\\s*>)|(<\\s*nocode)|(nocode <-)|(#\\s*nocode)", code_lines5))
  expect_true(no_excluded)


  code_lines6 <- get_code(
    files_path = file_path, dataname = "ADSL", exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]

  no_excluded <- any(!grepl("(nocode\\s*>)|(<\\s*nocode)|(nocode <-)|(#\\s*nocode)", code_lines6))
  expect_true(no_excluded)


  code_lines7 <- get_code(
    files_path = file_path, dataname = "ADSL", exclude_comments = TRUE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]

  no_excluded <- any(!grepl("(nocode\\s*>)|(<\\s*nocode)|(nocode <-)|(#\\s*nocode)", code_lines6))
  expect_true(no_excluded)
})


test_that("Include sourced code", {
  code_lines8 <- get_code(
    files_path = file_path, dataname = "ADTTE", exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]

  source1 <- get_code(
    files_path = file_path1, dataname = "ADTTE", exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]
  source2 <- get_code(
    files_path = file_path2, dataname = "ADTTE", exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]
  source3 <- get_code(
    files_path = file_path3, dataname = "ADTTE", exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]

  expect_true(all(c(source1, source2, source3) %in% code_lines8))
})


test_that("union diff", {
  ADTTE <- get_code( # nolint
    files_path = file_path, dataname = "ADTTE", exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]

  ADSL <- get_code( # nolint
    files_path = file_path, dataname = "ADSL", exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]

  ADSL2 <- get_code( # nolint
    files_path = file_path, dataname = "ADSL  ", exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]

  ALL <- get_code( # nolint
    files_path = file_path, dataname = "", exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]
  ALL2 <- get_code( # nolint
    files_path = file_path, dataname = NULL, exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]

  ALL3 <- get_code( # nolint
    files_path = file_path, dataname = NA, exclude_comments = FALSE,
    read_sources = TRUE
  ) %>%
    strsplit("\n") %>%
    .[[1]]

  expect_true(length(ALL) < sum(c(length(ADSL), length(ADTTE))))
  expect_true(all(ADSL %in% ALL))
  expect_true(all(ADTTE %in% ALL))
  expect_false(all(ADSL %in% ADTTE))
  expect_true(all(ALL %in% c(ADSL, ADTTE)))
  expect_true(identical(ADSL, ADSL2))
  expect_identical(ALL, ALL2)
  expect_identical(ALL, ALL3)
  expect_true(setequal(unique(c(ADSL, ADTTE)), ALL))
})

test_that("NULL empty", {
  expect_error(get_code(
    files_path = file_path, dataname = "wrongname", exclude_comments = FALSE,
    read_sources = TRUE
  ))

  expect_equal(
    get_code(
      files_path = file_path_nochunks, dataname = "wrongname", exclude_comments = FALSE,
      read_sources = TRUE
    ),
    "ADSL <- data.frame(1, 2) # nolint\n\nADSL$x <- 2 # nolint"
  )

  expect_equal(
    get_code(
      files_path = file_path_nochunks, dataname = "", exclude_comments = FALSE,
      read_sources = TRUE
    ),
    "ADSL <- data.frame(1, 2) # nolint\n\nADSL$x <- 2 # nolint"
  )
})
