testthat::test_that("The constructor does not throw", {
  testthat::expect_error(FilteredData$new(), NA)
})

testthat::test_that("datanames returns an empty array after initialization", {
  filtered_data <- FilteredData$new()
  testthat::expect_equal(filtered_data$datanames(), c())
})

testthat::test_that("set_dataset does not throw when passed a TealDataset object", {
  filtered_data <- FilteredData$new()
  testthat::expect_error(filtered_data$set_dataset(TealDataset$new("iris", head(iris))), NA)
})

testthat::test_that("set_dataset does throw when passed a TealDatasetConnector object before pulling it", {
  filtered_data <- FilteredData$new()
  testthat::expect_error(filtered_data$set_dataset(
    TealDatasetConnector$new("iris", callable_function(function() head(iris)))
  ), regexp = "has not been pulled yet")
})

testthat::test_that("set_dataset does not throw when passed a pulled TealDatasetConnector object", {
  filtered_data <- FilteredData$new()
  connector <- TealDatasetConnector$new("iris", callable_function(function() head(iris)))
  connector$pull()
  testthat::expect_error(filtered_data$set_dataset(connector, NA))
})

testthat::test_that("get_keys returns an empty character when a TealDataset has no keys", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("iris", head(iris)))
  testthat::expect_equal(filtered_data$get_keys("iris"), character(0))
})

test_that("get_keys returns the same character array if a TealDataset has keys", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("iris", head(iris), keys = c("test")))
  testthat::expect_equal(filtered_data$get_keys("iris"), "test")
})

testthat::test_that("get_varnames returns dataname's column names", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("iris", head(iris)))
  testthat::expect_equal(filtered_data$get_varnames("iris"), colnames(iris))
})

testthat::test_that("get_varlabels returns an array of NAs when a TealDataset has no variable labels", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("iris", head(iris)))
  testthat::expect_equal(
    filtered_data$get_varlabels("iris"),
    setNames(object = rep(as.character(NA), ncol(iris)), nm = colnames(iris))
  )
})

testthat::test_that("get_varlabels returns array's labels when a TealDataset has variable labels", {
  mock_iris <- head(iris)
  variable_labels(mock_iris) <- rep("test", ncol(mock_iris))
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("iris", mock_iris))
  testthat::expect_equal(
    filtered_data$get_varlabels("iris"),
    setNames(object = rep("test", ncol(mock_iris)), nm = colnames(mock_iris))
  )
})

testthat::test_that("get_datalabel returns character(0) for a TealDataset with no labels", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("iris", head(iris)))
  testthat::expect_equal(filtered_data$get_datalabel("iris"), character(0))
})

testthat::test_that("get_datalabel returns the label of a passed TealDataset", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("iris", head(iris), label = "test"))
  testthat::expect_equal(filtered_data$get_datalabel("iris"), "test")
})

testthat::test_that("set_code does not throw when passed a CodeClass", {
  filtered_data <- FilteredData$new()
  testthat::expect_error(filtered_data$set_code(CodeClass$new("'preprocessing code'")), NA)
})

testthat::test_that("get_code return the code passed to set_code", {
  filtered_data <- FilteredData$new()
  filtered_data$set_code(CodeClass$new("'preprocessing code'"))
  testthat::expect_equal(filtered_data$get_code(), "\"preprocessing code\"")
})

testthat::test_that("get_data does not throw when passed a dataset name", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("iris", head(iris)))
  testthat::expect_equal(isolate(filtered_data$get_data("iris")), head(iris))
})

testthat::test_that("get_filtered_dataset returns a list of FilteredDataset", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("iris", head(iris)))
  checkmate::expect_list(filtered_data$get_filtered_dataset(), "FilteredDataset")
})

testthat::test_that("get_filtered_dataset returns a list with elements named after set datasets", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("iris", head(iris)))
  filtered_data$set_dataset(TealDataset$new("mtcars", head(mtcars)))
  testthat::expect_equal(names(filtered_data$get_filtered_dataset()), c("iris", "mtcars"))
})

testthat::test_that("get_call returns a list of language objects", {
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("iris", head(iris)))
  checkmate::expect_list(filtered_data$get_call("iris"), types = "<-")
})

testthat::test_that("get call returns a call assigning the filtered object to <name>_FILTERED", {
  mock_iris <- head(iris)
  filtered_data <- FilteredData$new()
  filtered_data$set_dataset(TealDataset$new("mock_iris", mock_iris))
  eval(filtered_data$get_call("mock_iris")[[1]])
  testthat::expect_equal(mock_iris_FILTERED, mock_iris)
})

testthat::test_that(
  "FilteredData$set_filter_state sets filters in FilteredDataset specified by the named list",
  code = {
    datasets <- FilteredData$new()
    datasets$set_dataset(dataset("iris", iris))
    datasets$set_dataset(dataset("mtcars", mtcars))
    fs <- list(
      iris = list(
        Sepal.Length = list(c(5.1, 6.4)),
        Species = c("setosa", "versicolor")
      ),
      mtcars = list(
        cyl = c(4, 6),
        disp = default_filter()
      )
    )
    datasets$set_filter_state(state = fs)
    testthat::expect_equal(
      isolate(datasets$get_call("iris")),
      list(
        filter = quote(
          iris_FILTERED <- dplyr::filter( # nolint
            iris,
            Sepal.Length >= 5.1 & Sepal.Length <= 6.4 &
              Species %in% c("setosa", "versicolor")
          )
        )
      )
    )

    testthat::expect_equal(
      isolate(datasets$get_call("mtcars")),
      list(
        filter = quote(
          mtcars_FILTERED <- dplyr::filter( # nolint
            mtcars,
            cyl %in% c("4", "6") & (disp >= 71.1 & disp <= 472)
          )
        )
      )
    )
  }
)

testthat::test_that(
  "FilteredData$set_filter_state throws error with unnamed datasets list",
  code = {
    datasets <- FilteredData$new()
    datasets$set_dataset(dataset("iris", iris))
    datasets$set_dataset(dataset("mtcars", mtcars))
    fs <- list(
      list(
        Sepal.Length = list(c(5.1, 6.4)),
        Species = c("setosa", "versicolor")
      ),
      mtcars = list(
        cyl = c(4, 6),
        disp = default_filter()
      )
    )
    testthat::expect_error(datasets$set_filter_state(state = fs))
  }
)

testthat::test_that(
  "FilteredData$set_filter_state throws error with unnamed variables list",
  code = {
    datasets <- FilteredData$new()
    datasets$set_dataset(dataset("iris", iris))
    datasets$set_dataset(dataset("mtcars", mtcars))
    fs <- list(
      iris = list(
        list(c(5.1, 6.4)),
        Species = c("setosa", "versicolor")
      ),
      mtcars = list(
        cyl = c(4, 6),
        disp = default_filter()
      )
    )
    testthat::expect_error(datasets$set_filter_state(state = fs))
  }
)

testthat::test_that("FilteredData$get_filter_state returns list identical to input",
  code = {
    datasets <- FilteredData$new()
    datasets$set_dataset(dataset("iris", iris))
    datasets$set_dataset(dataset("mae", MultiAssayExperiment::miniACC))
    fs <- list(
      iris = list(
        Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
        Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
      ),
      mae = list(
        subjects = list(
          years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
          vital_status = list(selected = "1", keep_na = FALSE),
          gender = list(selected = "female", keep_na = TRUE)
        ),
        RPPAArray = list(
          subset = list(ARRAY_TYPE = list(selected = "", keep_na = TRUE))
        )
      )
    )
    datasets$set_filter_state(state = fs)
    testthat::expect_identical(isolate(datasets$get_filter_state()), fs)
  }
)

testthat::test_that("FilteredData$remove_filter_state removes states defined in list", {
  datasets <- teal:::FilteredData$new()
  datasets$set_dataset(dataset("iris", iris))
  datasets$set_dataset(dataset("mtcars", mtcars))
  fs <- list(
    iris = list(
      Sepal.Length = list(c(5.1, 6.4)),
      Species = c("setosa", "versicolor")
    ),
    mtcars = list(
      cyl = c(4, 6),
      disp = default_filter()
    )
  )
  datasets$set_filter_state(state = fs)
  datasets$remove_filter_state(state = list(iris = "Sepal.Length", mtcars = c("cyl", "disp")))

  testthat::expect_identical(
    isolate(datasets$get_filter_state()),
    list(
      iris = list(
        Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
      )
    )
  )
})

testthat::test_that(
  "FilteredData$remove_all_filter_states removes all filters of all datasets in FilteredData",
  code = {
    datasets <- FilteredData$new()
    datasets$set_dataset(dataset("iris", iris))
    datasets$set_dataset(dataset("mtcars", mtcars))
    fs <- list(
      iris = list(
        Sepal.Length = list(c(5.1, 6.4)),
        Species = c("setosa", "versicolor")
      ),
      mtcars = list(
        cyl = c(4, 6),
        disp = default_filter()
      )
    )
    datasets$set_filter_state(state = fs)
    datasets$remove_all_filter_states()

    testthat::expect_equal(
      isolate(datasets$get_call("iris")),
      list(filter = quote(iris_FILTERED <- iris)) # nolint
    )

    testthat::expect_equal(
      isolate(datasets$get_call("mtcars")),
      list(
        filter = quote(
          mtcars_FILTERED <- mtcars # nolint
        )
      )
    )
  }
)

testthat::test_that(
  "FilteredData$remove_all_filter_states remove the filters of the desired dataset only",
  code = {
    datasets <- FilteredData$new()
    datasets$set_dataset(dataset("iris", iris))
    datasets$set_dataset(dataset("mtcars", mtcars))
    fs <- list(
      iris = list(
        Sepal.Length = list(c(5.1, 6.4)),
        Species = c("setosa", "versicolor")
      ),
      mtcars = list(
        cyl = c(4, 6),
        disp = default_filter()
      )
    )
    datasets$set_filter_state(state = fs)
    datasets$remove_all_filter_states(datanames = "iris")

    testthat::expect_equal(
      isolate(datasets$get_call("iris")),
      list(filter = quote(iris_FILTERED <- iris)) # nolint
    )

    testthat::expect_equal(
      isolate(datasets$get_call("mtcars")),
      list(
        filter = quote(
          mtcars_FILTERED <- dplyr::filter( # nolint
            mtcars,
            cyl %in% c("4", "6") & (disp >= 71.1 & disp <= 472)
          )
        )
      )
    )
  }
)

datasets <- FilteredData$new()
adsl <- as.data.frame(as.list(setNames(nm = c(get_cdisc_keys("ADSL")))))
adsl$sex <- c("F")
datasets$set_dataset(cdisc_dataset("ADSL", adsl))
datasets$set_dataset(dataset("mock_iris", head(iris)))
datasets$set_dataset(dataset("miniACC", MultiAssayExperiment::miniACC))

testthat::test_that("get_filter_overview accepts all datasets argument input", {
  testthat::expect_error(isolate(datasets$get_filter_overview("all")), NA)
})

testthat::test_that("get_filter_overview accepts single dataset argument input", {
  testthat::expect_error(isolate(datasets$get_filter_overview("ADSL")), NA)
  testthat::expect_error(isolate(datasets$get_filter_overview("mock_iris")), NA)
  testthat::expect_error(isolate(datasets$get_filter_overview("miniACC")), NA)
})

testthat::test_that("get_filter_overview throws error with empty argument input", {
  testthat::expect_error(isolate(datasets$get_filter_overview()), "argument \"datanames\" is missing, with no default")
})

testthat::test_that("get_filter_overview throws error with wrong argument input", {
  testthat::expect_error(isolate(datasets$get_filter_overview("AA")), "Some datasets are not available:")
  testthat::expect_error(isolate(datasets$get_filter_overview("")), "Some datasets are not available:")
  testthat::expect_error(isolate(datasets$get_filter_overview(23)), "Some datasets are not available:")
})

testthat::test_that("get_filter_overview returns overview matrix for non-filtered datasets", {
  testthat::expect_equal(
    isolate(datasets$get_filter_overview(datasets$datanames())),
    matrix(
      list(
        "1/1", "1/1", "6/6", "", "", "92/92", "79/79", "79/79", "90/90",
        "90/90", "46/46", "46/46", "90/90", "90/90", "80/80", "80/80"
      ),
      nrow = 8,
      byrow = TRUE,
      dimnames = list(
        c(
          "ADSL", "mock_iris", "miniACC", "- RNASeq2GeneNorm", "- gistict",
          "- RPPAArray", "- Mutations", "- miRNASeqGene"
        ),
        c("Obs", "Subjects")
      )
    )
  )
})

testthat::test_that("get_filter_overview returns overview matrix for filtered datasets", {
  filter_state_adsl <- ChoicesFilterState$new(c("F", "M"), varname = "sex")
  filter_state_adsl$set_selected("M")
  queue <- datasets$get_filtered_dataset("ADSL")$get_filter_states(1)
  queue$queue_push(filter_state_adsl, queue_index = 1L, element_id = "sex")
  filter_state_mae <- ChoicesFilterState$new(
    x = c("white", NA),
    varname = as.name("race"),
    input_dataname = as.name("miniACC"),
    extract_type = "list"
  )
  filter_state_mae$set_na_rm(TRUE)
  queue <- datasets$get_filtered_dataset("miniACC")$get_filter_states(1)
  queue$queue_push(filter_state_mae, queue_index = 1L, element_id = "race")
  testthat::expect_equal(
    isolate(datasets$get_filter_overview(datasets$datanames())),
    matrix(
      list(
        "0/1", "0/1", "6/6", "", "", "78/92", "66/79", "66/79", "76/90",
        "76/90", "35/46", "35/46", "77/90", "77/90", "67/80", "67/80"
      ),
      nrow = 8,
      byrow = TRUE,
      dimnames = list(
        c(
          "ADSL", "mock_iris", "miniACC", "- RNASeq2GeneNorm", "- gistict",
          "- RPPAArray", "- Mutations", "- miRNASeqGene"
        ),
        c("Obs", "Subjects")
      )
    )
  )
})

testthat::test_that("restore_state_from_bookmark is a pure virtual method", {
  testthat::expect_error(
    FilteredData$new()$restore_state_from_bookmark("test"),
    regexp = "Pure virtual method"
  )
})

testthat::test_that("get_filter_expr returns a string with a filtering expression", {
  datasets <- FilteredData$new()
  datasets$set_dataset(dataset("iris", iris, code = "iris <- iris"))
  datasets$set_dataset(dataset("mtcars", mtcars, code = "mtcars <- mtcars"))
  testthat::expect_equal(
    get_filter_expr(datasets),
    paste("iris_FILTERED <- iris", "mtcars_FILTERED <- mtcars", sep = "\n")
  )
})
