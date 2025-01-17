testthat::describe("srv_session_info lockfile", {
  testthat::it(paste0(
    "creation process is invoked for teal.lockfile.mode = \"enabled\" ",
    "and snapshot is copied to teal_app.lock and removed after session ended"
  ), {
    testthat::skip_if_not_installed("mirai")
    testthat::skip_if_not_installed("renv")
    withr::with_options(
      list(teal.lockfile.mode = "enabled"),
      {
        renv_filename <- "teal_app.lock"
        shiny::testServer(
          app = srv_session_info,
          args = list(id = "test"),
          expr = {
            iter <- 1
            while (!file.exists(renv_filename) && iter <= 1000) {
              Sys.sleep(0.5)
              iter <- iter + 1 # max wait time is 500 seconds
            }
            testthat::expect_true(file.exists(renv_filename))
          }
        )
        testthat::expect_false(file.exists(renv_filename))
      }
    )
  })
  testthat::it("creation process is not invoked for teal.lockfile.mode = \"disabled\"", {
    testthat::skip_if_not_installed("mirai")
    testthat::skip_if_not_installed("renv")
    withr::with_options(
      list(teal.lockfile.mode = "disabled"),
      {
        renv_filename <- "teal_app.lock"
        shiny::testServer(
          app = srv_session_info,
          args = list(id = "test"),
          expr = {
            testthat::expect_false(file.exists(renv_filename))
          }
        )
      }
    )
  })
})
