testthat::test_that("get_labels returns a list with two keys", {
  testthat::expect_equal(names(get_labels(iris, fill = TRUE)), c("dataset_label", "column_labels"))
})

testthat::test_that("get_labels accepts an empty data.frame", {
  testthat::expect_error(get_labels(data.frame()), regexp = NA)
})

testthat::test_that("get_labels' dataset_label is NULL if the dataset has no label attribute", {
  testthat::expect_null(get_labels(iris, fill = TRUE)$dataset_label)
})

testthat::test_that("get_labels' dataset_label is equal to the label attribute of the passed data.frame", {
  custom_iris <- iris
  attributes(custom_iris)$label <- "Test"
  testthat::expect_equal(get_labels(custom_iris, fill = TRUE)$dataset_label, "Test")
})

testthat::test_that("get_labels' column_labels is NULL for a data.frame with no columns", {
  testthat::expect_null(get_labels(data.frame()[1:5, ], fill = TRUE)$column_labels)
})

testthat::test_that("get_labels' column_labels is a named vector of NA when fill = FALSE and there are no labels", {
  testthat::expect_equal(
    get_labels(iris, fill = FALSE)$column_labels,
    stats::setNames(rep(NA_character_, times = ncol(iris)), nm = colnames(iris))
  )
})

testthat::test_that("get_labels' column labels is a vector of column names when fill = TRUE and there are no labels", {
  testthat::expect_equal(
    get_labels(iris, fill = TRUE)$column_labels,
    stats::setNames(colnames(iris), nm = colnames(iris))
  )
})

testthat::test_that("variable_labels<- assigns the labels to the columns", {
  custom_iris <- iris
  variable_labels(custom_iris) <- colnames(custom_iris)
  testthat::expect_equal(get_variable_labels(custom_iris, fill = FALSE), stats::setNames(nm = colnames(custom_iris)))
})
