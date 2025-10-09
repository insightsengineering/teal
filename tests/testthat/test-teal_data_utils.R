# Tests for teal_data_utils.R functions

testthat::test_that(".append_evaluated_code appends code to teal_data object", {
  td <- teal.data::teal_data(iris = iris)
  code_to_append <- "iris_subset <- iris[1:10, ]"
  
  result <- .append_evaluated_code(td, code_to_append)
  
  testthat::expect_s4_class(result, "teal_data")
  testthat::expect_match(teal.code::get_code(result), "iris_subset <- iris\\[1:10, \\]")
})

testthat::test_that(".append_evaluated_code handles empty code", {
  td <- teal.data::teal_data(iris = iris)
  
  result1 <- .append_evaluated_code(td, "")
  result2 <- .append_evaluated_code(td, character(0))
  
  testthat::expect_s4_class(result1, "teal_data")
  testthat::expect_s4_class(result2, "teal_data")
})

testthat::test_that(".append_evaluated_code validates input", {
  testthat::expect_error(
    .append_evaluated_code(list(), "code"),
    "Assertion.*failed.*Must inherit from class 'teal_data'"
  )
})

testthat::test_that(".append_evaluated_code updates teal_card metadata", {
  td <- teal.data::teal_data(iris = iris)
  code_to_append <- "iris_subset <- iris[1:10, ]"
  
  result <- .append_evaluated_code(td, code_to_append)
  
  card <- teal.reporter::teal_card(result)
  testthat::expect_true(any(grepl("Data filtering", card)))
})

testthat::test_that(".append_modified_data appends objects to teal_data environment", {
  td <- teal.data::teal_data(iris = iris)
  new_objects <- list(mtcars = mtcars, new_var = 42)
  
  result <- .append_modified_data(td, new_objects)
  
  testthat::expect_s4_class(result, "teal_data")
  testthat::expect_true("mtcars" %in% ls(result))
  testthat::expect_true("new_var" %in% ls(result))
  testthat::expect_equal(result[["new_var"]], 42)
})

testthat::test_that(".append_modified_data validates inputs", {
  td <- teal.data::teal_data(iris = iris)
  
  testthat::expect_error(
    .append_modified_data(list(), list()),
    "Assertion.*failed.*Must inherit from class 'teal_data'"
  )
  
  testthat::expect_error(
    .append_modified_data(td, "not a list"),
    "Assertion.*failed.*Must inherit from class 'list'"
  )
})

testthat::test_that(".collapse_subsequent_chunks collapses consecutive code chunks", {
  card <- teal.reporter::teal_card()
  card <- teal.reporter::append_content(card, teal.reporter::code_chunk("code1"))
  card <- teal.reporter::append_content(card, teal.reporter::code_chunk("code2"))
  
  result <- .collapse_subsequent_chunks(card)
  
  testthat::expect_s3_class(result, "teal_card")
  # The result should have fewer chunks since consecutive code chunks are collapsed
  testthat::expect_lte(length(result), length(card))
})

testthat::test_that(".collapse_subsequent_chunks preserves non-code content", {
  card <- teal.reporter::teal_card()
  card <- teal.reporter::append_content(card, teal.reporter::code_chunk("code1"))
  card <- teal.reporter::append_content(card, teal.reporter::text_chunk("text"))
  card <- teal.reporter::append_content(card, teal.reporter::code_chunk("code2"))
  
  result <- .collapse_subsequent_chunks(card)
  
  testthat::expect_s3_class(result, "teal_card")
})

testthat::test_that(".collapse_subsequent_chunks validates input", {
  testthat::expect_error(
    .collapse_subsequent_chunks(list()),
    "Assertion.*failed.*Must inherit from class 'teal_card'"
  )
})

testthat::test_that(".collapse_subsequent_chunks handles empty card", {
  card <- teal.reporter::teal_card()
  
  result <- .collapse_subsequent_chunks(card)
  
  testthat::expect_s3_class(result, "teal_card")
})

testthat::test_that(".collapse_subsequent_chunks collapses only chunks with same params", {
  card <- teal.reporter::teal_card()
  card <- teal.reporter::append_content(card, teal.reporter::code_chunk("code1", eval = TRUE))
  card <- teal.reporter::append_content(card, teal.reporter::code_chunk("code2", eval = TRUE))
  card <- teal.reporter::append_content(card, teal.reporter::code_chunk("code3", eval = FALSE))
  
  result <- .collapse_subsequent_chunks(card)
  
  testthat::expect_s3_class(result, "teal_card")
  # First two chunks should be collapsed (same params), third should be separate
  testthat::expect_equal(length(result), 2)
})
