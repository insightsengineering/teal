output_table <- function(output) {
  as.data.frame(rvest::html_table(rvest::read_html(output$table$html))[[1]])
}

testthat::test_that("srv_data_summary calculates filter_overview properly for a single dataset ", {
  data <- teal.data::teal_data(iris_raw = iris, iris = iris[1:50, ])
  teal.data::datanames(data) <- 'iris'

    shiny::testServer(
      app = srv_data_summary,
      args = list(
        id = "test",
        teal_data = reactive(data)
      ),
      expr = {
        testthat::expect_equal(
          get_filter_overview(teal_data),
          data.frame(
            dataname = 'iris',
            obs = 150,
            obs_filtered = 50,
            subjects = NA,
            subjects_filtered = NA
          )
        )
      }
   )
})

testthat::test_that("srv_data_summary does not produce subjects column if there is no join keys", {
  data <- teal.data::teal_data(iris_raw = iris, iris = iris[1:50, ])
  teal.data::datanames(data) <- 'iris'

  shiny::testServer(
    app = srv_data_summary,
    args = list(
      id = "test",
      teal_data = reactive(data)
    ),
    expr = {
      testthat::expect_equal(
        output_table(output),
        data.frame(
          X1 = c('Data Name', 'iris'),
          X2 = c('Obs', '50/150')
        )
      )
    }
  )
})

testthat::test_that("srv_data_summary produces subjects column if there are join keys", {
  data <- teal.data::teal_data(
    mtcars1 = mtcars[1:30,],
    mtcars1_raw = mtcars,
    mtcars2 = data.frame(am = c(0,1), test = c('a', 'b')),
    mtcars2_raw = data.frame(am = c(0,1), test = c('a', 'b'))
  )
  teal.data::datanames(data) <- c('mtcars1', 'mtcars2')

  teal.data::join_keys(data) <- teal.data::join_keys(
    teal.data::join_key('mtcars2', 'mtcars1', keys = c('am'))
  )

  shiny::testServer(
    app = srv_data_summary,
    args = list(
      id = "test",
      teal_data = reactive(data)
    ),
    expr = {
      testthat::expect_equal(
        output_table(output),
        data.frame(
          X1 = c('Data Name', 'mtcars1', 'mtcars2'),
          X2 = c('Obs', '30/32', '2/2'),
          X3 = c('Subjects', '2/2', '2/2')
        )
      )
    }
  )
})

testthat::test_that("srv_data_summary filter_overview returns NULL for empty datanames", {
  data <- teal.data::teal_data(iris_raw = iris, iris = iris[1:50, ])
  teal.data::datanames(data) <- character(0)

  shiny::testServer(
    app = srv_data_summary,
    args = list(
      id = "test",
      teal_data = reactive(data)
    ),
    expr = {
      testthat::expect_null(
        get_filter_overview(teal_data)
      )
    }
  )
})

testthat::test_that("srv_data_summary crashes if there is data, but not filtered_data (raw)", {
  data <- teal.data::teal_data(iris = iris)

  testthat::expect_error(
    shiny::testServer(
      app = srv_data_summary,
      args = list(
        id = "test",
        teal_data = reactive(data)
      ),
      expr = output$table
    ),
    'arguments imply differing number of rows'
  )
})


testthat::test_that("srv_data_summary calculates counts properly for mixture of MAE dataset, dataframes and vectors", {
  testthat::skip_if_not_installed("MultiAssayExperiment")
  data <- teal.data::teal_data() %>%
    within(
      {
        mtcars1 <- mtcars[1:30, ]
        mtcars1_raw <- mtcars
        mtcars2 <- data.frame(am = c(0,1), test = c('a', 'b'))
        mtcars2_raw <- data.frame(am = c(0,1), test = c('a', 'b'))
        iris <- iris[1:50, ]
        iris_raw <- iris
        library(MultiAssayExperiment)
        data("miniACC", package = "MultiAssayExperiment", envir = environment())
        miniACC_raw <- miniACC
        CO2_raw <- CO2 <- CO2
        factors_raw <- factors <- names(Filter(isTRUE, vapply(CO2, is.factor, logical(1L))))
        CO2[factors] <- lapply(CO2[factors], as.character)
      }
    )

  teal.data::join_keys(data) <- teal.data::join_keys(
    teal.data::join_key('mtcars2', 'mtcars1', keys = c('am'))
  )

  teal.data::datanames(data) <- c('mtcars1', 'mtcars2', 'iris', 'miniACC', 'CO2', 'factors')

  shiny::testServer(
    app = srv_data_summary,
    args = list(
      id = "test",
      teal_data = reactive(data)
    ),
    expr = {
      testthat::expect_equal(
        output_table(output),
        data.frame(
          X1 = c(
            "Data Name", "mtcars1", "mtcars2", "iris", "miniACC", "- RNASeq2GeneNorm", "- gistict", "- RPPAArray",
            "- Mutations", "- miRNASeqGene", "CO2", "factors"
          ),
          X2 = c("Obs", "30/32", "2/2", "50/50", "", "198/198", "198/198", "33/33", "97/97", "471/471", "84/84", ""),
          X3 = c("Subjects", "2/2", "2/2", "", "92/92", "79/79", "90/90", "46/46", "90/90", "80/80", "", "")
        )
      )
    }
  )
})
