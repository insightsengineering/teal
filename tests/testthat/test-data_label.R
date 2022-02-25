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
  testthat::expect_equal(variable_labels(custom_iris, fill = FALSE), stats::setNames(nm = colnames(custom_iris)))
})

testthat::test_that("get_labels' column_labels is a named vector of the labels of the passed data.frame", {
  test <- data.frame(a = 1, b = 2)
  test_labels <- c("testa", "testb")
  attr(test[[1]], "label") <- test_labels[1]
  attr(test[[2]], "label") <- test_labels[2]
  testthat::expect_equal(get_labels(test)$column_labels, stats::setNames(object = test_labels, nm = colnames(test)))
})

testthat::test_that("variable_labels<- does not assign NA labels", {
  test_iris <- iris
  duplicate_test_iris <- test_iris

  variable_labels(test_iris) <- variable_labels(test_iris, fill = FALSE)
  testthat::expect_equal(test_iris, duplicate_test_iris)
})

testthat::test_that("var_relabel returns the unchanged data.frame if passed nothing but a data.frame", {
  testthat::expect_equal(var_relabel(iris), iris)
})

testthat::test_that("var_relabel adds a label attribute to columns", {
  labeled_iris <- var_relabel(iris, "Sepal.Length" = "Test")
  testthat::expect_equal(attr(labeled_iris[["Sepal.Length"]], "label"), "Test")
})

testthat::test_that("var_relabel asserts the first argument is a data.frame", {
  testthat::expect_error(var_relabel("test"), regexp = "Must be of type 'data\\.frame'")
})

testthat::test_that("var_relabel asserts the passed label is a character", {
  testthat::expect_error(
    var_relabel(iris, "Sepal.Length" = 8),
    regexp = "May only contain the following types: \\{character\\}, but element 1 has type 'numeric'"
  )
})

testthat::test_that("var_relabel asserts that column names are not NAs", {
  testthat::expect_error(
    var_relabel(iris, `NA` = "test"),
    regexp = "Contains missing values \\(element 1\\)"
  )
})
