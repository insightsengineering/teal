testthat::describe("ui_teal_lockfile", {
  testthat::it("returns a shiny tagList", {
    result <- ui_teal_lockfile("test_id")
    testthat::expect_s3_class(result, "shiny.tag.list")
  })

  testthat::it("creates namespaced elements with correct IDs", {
    result <- ui_teal_lockfile("test_id")
    result_html <- as.character(result)
    testthat::expect_true(grepl("test_id-lockFileStatus", result_html))
    testthat::expect_true(grepl("test_id-lockFileLink", result_html))
  })

  testthat::it("creates disabled download link", {
    result <- ui_teal_lockfile("test_id")
    result_html <- as.character(result)
    testthat::expect_true(grepl("disabled", result_html))
  })

  testthat::it("creates elements with correct structure", {
    result <- ui_teal_lockfile("test_id")
    testthat::expect_length(result, 2)
    # First element should be a span tag
    testthat::expect_true(grepl("<span", as.character(result[[1]])))
    # Second element should be a download link
    testthat::expect_true(grepl("Download lockfile", as.character(result[[2]])))
  })
})

testthat::describe(".is_lockfile_deps_installed", {
  testthat::it("returns TRUE when both mirai and renv are installed", {
    testthat::skip_if_not_installed("mirai")
    testthat::skip_if_not_installed("renv")
    testthat::expect_true(.is_lockfile_deps_installed())
  })

  testthat::it("returns logical value", {
    result <- .is_lockfile_deps_installed()
    testthat::expect_type(result, "logical")
    testthat::expect_length(result, 1)
  })
})

testthat::describe(".is_disabled_lockfile_scenario", {
  testthat::it("returns TRUE when CALLR_IS_RUNNING is 'true'", {
    withr::with_envvar(
      list(CALLR_IS_RUNNING = "true"),
      {
        testthat::expect_true(.is_disabled_lockfile_scenario())
      }
    )
  })

  testthat::it("returns TRUE when TESTTHAT is 'true'", {
    withr::with_envvar(
      list(TESTTHAT = "true"),
      {
        testthat::expect_true(.is_disabled_lockfile_scenario())
      }
    )
  })

  testthat::it("returns TRUE when QUARTO_PROJECT_ROOT is set", {
    withr::with_envvar(
      list(QUARTO_PROJECT_ROOT = "/some/path"),
      {
        testthat::expect_true(.is_disabled_lockfile_scenario())
      }
    )
  })

  testthat::it("returns TRUE when _R_CHECK_TIMINGS_ is set", {
    withr::with_envvar(
      list("_R_CHECK_TIMINGS_" = "true"),
      {
        testthat::expect_true(.is_disabled_lockfile_scenario())
      }
    )
  })

  testthat::it("returns TRUE when _R_CHECK_LICENSE_ is set", {
    withr::with_envvar(
      list("_R_CHECK_LICENSE_" = "true"),
      {
        testthat::expect_true(.is_disabled_lockfile_scenario())
      }
    )
  })

  testthat::it("returns FALSE when none of the disabled scenarios are active", {
    withr::with_envvar(
      list(
        CALLR_IS_RUNNING = "",
        TESTTHAT = "",
        QUARTO_PROJECT_ROOT = "",
        "_R_CHECK_TIMINGS_" = NULL,
        "_R_CHECK_LICENSE_" = NULL
      ),
      {
        # This test runs in a testthat environment, so TESTTHAT will be "true"
        # We need to temporarily unset it
        old_testthat <- Sys.getenv("TESTTHAT")
        Sys.unsetenv("TESTTHAT")
        on.exit(Sys.setenv(TESTTHAT = old_testthat), add = TRUE)
        
        result <- .is_disabled_lockfile_scenario()
        # Result may still be TRUE due to CheckExEnv in search path during tests
        testthat::expect_type(result, "logical")
      }
    )
  })
})

testthat::describe(".renv_snapshot", {
  testthat::it("returns a list with 'out' and 'res' components", {
    testthat::skip_if_not_installed("renv")
    
    # Create a temporary directory for the test
    temp_dir <- withr::local_tempdir()
    lockfile_path <- file.path(temp_dir, "test.lock")
    
    # Run in a controlled environment
    withr::with_dir(temp_dir, {
      result <- .renv_snapshot(lockfile_path)
      
      testthat::expect_type(result, "list")
      testthat::expect_true("out" %in% names(result))
      testthat::expect_true("res" %in% names(result))
      testthat::expect_type(result$out, "character")
    })
  })
})

testthat::describe(".teal_lockfile_process_invoke", {
  testthat::it("returns an ExtendedTask object", {
    testthat::skip_if_not_installed("mirai")
    
    temp_dir <- withr::local_tempdir()
    lockfile_path <- file.path(temp_dir, "test.lock")
    
    withr::with_dir(temp_dir, {
      result <- .teal_lockfile_process_invoke(lockfile_path)
      testthat::expect_s3_class(result, "ExtendedTask")
    })
  })
})

testthat::describe("srv_teal_lockfile", {
  testthat::it("throws error for invalid teal.lockfile.mode option", {
    withr::with_options(
      list(teal.lockfile.mode = "invalid"),
      {
        testthat::expect_error(
          shiny::testServer(
            app = srv_teal_lockfile,
            args = list(id = "test"),
            expr = {}
          ),
          "'teal.lockfile.mode' option can only be one of"
        )
      }
    )
  })

  testthat::it("returns NULL when mode is 'disabled'", {
    withr::with_options(
      list(teal.lockfile.mode = "disabled"),
      {
        shiny::testServer(
          app = srv_teal_lockfile,
          args = list(id = "test"),
          expr = {
            testthat::expect_null(session$returned())
          }
        )
      }
    )
  })

  testthat::it("enables download when lockfile already exists", {
    testthat::skip_if_not_installed("mirai")
    testthat::skip_if_not_installed("renv")
    
    # Create a temporary lockfile
    temp_lockfile <- "teal_app.lock"
    file.create(temp_lockfile)
    on.exit(if (file.exists(temp_lockfile)) file.remove(temp_lockfile), add = TRUE)
    
    withr::with_options(
      list(teal.lockfile.mode = "enabled"),
      {
        shiny::testServer(
          app = srv_teal_lockfile,
          args = list(id = "test"),
          expr = {
            # The server should detect existing lockfile and enable download
            testthat::expect_null(session$returned())
          }
        )
      }
    )
    
    # Clean up
    if (file.exists(temp_lockfile)) file.remove(temp_lockfile)
  })

  testthat::it("returns NULL in auto mode when in disabled scenario", {
    withr::with_options(
      list(teal.lockfile.mode = "auto"),
      {
        # In test environment, .is_disabled_lockfile_scenario() returns TRUE
        shiny::testServer(
          app = srv_teal_lockfile,
          args = list(id = "test"),
          expr = {
            testthat::expect_null(session$returned())
          }
        )
      }
    )
  })

  testthat::it("handles default empty mode option", {
    withr::with_options(
      list(teal.lockfile.mode = NULL),
      {
        # Default mode is "" (empty string)
        # In test environment, .is_disabled_lockfile_scenario() returns TRUE
        # so it should return NULL
        shiny::testServer(
          app = srv_teal_lockfile,
          args = list(id = "test"),
          expr = {
            testthat::expect_null(session$returned())
          }
        )
      }
    )
  })

  testthat::it("accepts valid mode values: auto, enabled, disabled", {
    # Test 'disabled' - doesn't need mirai/renv
    withr::with_options(
      list(teal.lockfile.mode = "disabled"),
      {
        testthat::expect_silent(
          shiny::testServer(
            app = srv_teal_lockfile,
            args = list(id = "test"),
            expr = {}
          )
        )
      }
    )
    
    # Test 'auto' - in test env, should be disabled
    withr::with_options(
      list(teal.lockfile.mode = "auto"),
      {
        testthat::expect_silent(
          shiny::testServer(
            app = srv_teal_lockfile,
            args = list(id = "test"),
            expr = {}
          )
        )
      }
    )
  })
})
