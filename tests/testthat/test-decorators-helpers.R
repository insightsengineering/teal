ttm <- teal_transform_module(server = function(id, data) data)

testthat::describe("check_decorators", {
  testthat::describe("returns TRUE when x is a valid named list of teal_transform_module objects", {
    it("accepts a named list with a single teal_transform_module", {
      testthat::expect_true(check_decorators(list(a = ttm)))
    })

    it("accepts a named list with multiple teal_transform_module objects", {
      testthat::expect_true(check_decorators(list(a = ttm, b = ttm)))
    })

    it("accepts a named list whose elements are lists of teal_transform_module objects", {
      testthat::expect_true(check_decorators(list(a = list(ttm, ttm))))
    })

    it("accepts a named list mixing a teal_transform_module and a list of teal_transform_module objects", {
      testthat::expect_true(check_decorators(list(a = ttm, b = list(ttm, ttm))))
    })

    it("accepts an empty list", {
      testthat::expect_true(check_decorators(list()))
    })
  })

  testthat::describe("returns TRUE when names argument is provided and all names are valid", {
    it("accepts 'all' as a name when names argument is provided", {
      testthat::expect_true(check_decorators(list(all = ttm), names = "output1"))
    })

    it("accepts a name that is present in the names argument", {
      testthat::expect_true(check_decorators(list(output1 = ttm), names = c("output1", "output2")))
    })

    it("accepts a mix of 'all' and a name from the names argument", {
      testthat::expect_true(
        check_decorators(list(all = ttm, output1 = ttm), names = c("output1", "output2"))
      )
    })

    it("accepts all provided names used together", {
      testthat::expect_true(
        check_decorators(list(output1 = ttm, output2 = ttm), names = c("output1", "output2"))
      )
    })
  })

  testthat::describe("returns a character string when x fails the basic list check", {
    it("returns a string when x is not a list", {
      result <- check_decorators("not_a_list")
      testthat::expect_match(result, "Must be of type 'list'", fixed = TRUE)
    })

    it("returns a string when x is an unnamed list", {
      result <- check_decorators(list(ttm))
      testthat::expect_match(result, "Must have names", fixed = TRUE)
    })
  })

  testthat::describe("returns a character string when elements are not teal_transform_module objects", {
    it("returns the expected message when a top-level element is not a teal_transform_module", {
      result <- check_decorators(list(a = "not_a_module"))
      testthat::expect_match(
        result,
        "teal_transform_module",
        fixed = TRUE
      )
    })

    it("returns the expected message when a nested list element is not a teal_transform_module", {
      result <- check_decorators(list(a = list("not_a_module")))
      testthat::expect_match(
        result,
        "teal_transform_module",
        fixed = TRUE
      )
    })
  })

  testthat::describe("returns a character string when names argument is provided and names in x are invalid", {
    it("returns the expected message when a name in x is not 'all' or in names argument", {
      result <- check_decorators(list(invalid_name = ttm), names = c("output1", "output2"))
      testthat::expect_match(result, "'all'", fixed = TRUE)
      testthat::expect_match(result, "output1", fixed = TRUE)
      testthat::expect_match(result, "output2", fixed = TRUE)
    })

    it("returns the expected message when names in x are duplicated", {
      result <- check_decorators(list(a = ttm, a = ttm), names = c("a", "b"))
      testthat::expect_match(result, "unique names", fixed = TRUE)
    })
  })
})

testthat::describe("assert_decorators", {
  testthat::describe("returns x invisibly when decorators are valid", {
    it("returns the input list invisibly when x contains a valid teal_transform_module", {
      x <- list(a = ttm)
      testthat::expect_identical(assert_decorators(x), x)
    })

    it("returns the input list invisibly when x contains valid nested decorators", {
      x <- list(a = list(ttm, ttm))
      testthat::expect_identical(assert_decorators(x), x)
    })

    it("returns the input list invisibly when names argument is provided and all names are valid", {
      x <- list(all = ttm, output1 = ttm)
      testthat::expect_identical(assert_decorators(x, names = c("output1", "output2")), x)
    })
  })

  testthat::describe("throws", {
    it("throws when x is not a named list", {
      testthat::expect_error(assert_decorators(list(ttm)))
    })

    it("throws when an element is not a teal_transform_module", {
      testthat::expect_error(assert_decorators(list(a = "not_a_module")))
    })

    it("throws when a name in x is not 'all' or in the names argument", {
      testthat::expect_error(assert_decorators(list(invalid_name = ttm), names = c("output1")))
    })

    it("throws when names in x are duplicated", {
      testthat::expect_error(assert_decorators(list(a = ttm, a = ttm), names = c("a", "b")))
    })
  })

  testthat::describe("collects errors in an AssertCollection when add is provided", {
    it("adds an error to the collection instead of raising when x is not a named list", {
      coll <- checkmate::makeAssertCollection()
      assert_decorators(list(ttm), add = coll)
      testthat::expect_false(coll$isEmpty())
    })

    it("adds an error to the collection instead of raising when an element is not a teal_transform_module", {
      coll <- checkmate::makeAssertCollection()
      assert_decorators(list(a = "not_a_module"), add = coll)
      testthat::expect_false(coll$isEmpty())
    })

    it("does not add to the collection when decorators are valid", {
      coll <- checkmate::makeAssertCollection()
      assert_decorators(list(a = ttm), add = coll)
      testthat::expect_true(coll$isEmpty())
    })
  })
})

# Test internal function reused on other packages teal.modules.general, teal.modules.clinical, ...
testthat::describe("select_decorators", {
  it("returns only 'all' decorators when the scope is 'all'", {
    decorators <- list(all = teal_transform_module(), plot = teal_transform_module())
    dec_out <- select_decorators(decorators, "all")
    checkmate::expect_list(dec_out, types = "teal_transform_module", len = 1L)
    testthat::expect_equivalent(dec_out, decorators["all"])
  })

  it("combines 'all' and scope-specific decorators when a specific scope is requested", {
    decorators <- list(all = teal_transform_module(), plot = teal_transform_module())
    dec_out <- select_decorators(decorators, "plot")
    checkmate::expect_list(dec_out, types = "teal_transform_module", len = 2L)
    testthat::expect_equivalent(dec_out, decorators)
  })

  it("returns only 'all' decorators when an unknown scope is provided", {
    decorators <- list(all = teal_transform_module(), plot = teal_transform_module())
    dec_out <- select_decorators(decorators, "other")
    checkmate::expect_list(dec_out, types = "teal_transform_module", len = 1L)
  })

  it("flattens a mixture of single modules and lists of modules", {
    decorators <- list(all = teal_transform_module(), plot = teal_transform_module())
    dec_out <- select_decorators(decorators, "plot")
    checkmate::expect_list(dec_out, types = "teal_transform_module", len = 2L)
    testthat::expect_equivalent(dec_out, decorators)
  })

  it("recursively flattens nested lists of decorators for the requested scope", {
    decorators <- list(
      all = list(teal_transform_module(), teal_transform_module()),
      plot = list(teal_transform_module(), teal_transform_module())
    )
    dec_out <- select_decorators(decorators, "plot")
    checkmate::expect_list(dec_out, types = "teal_transform_module", len = 4L)
  })

  it("recursively flattens when only the requested scope contains multiple modules", {
    decorators <- list(all = teal_transform_module(), plot = list(teal_transform_module(), teal_transform_module()))
    dec_out <- select_decorators(decorators, "plot")
    checkmate::expect_list(dec_out, types = "teal_transform_module", len = 3L)
  })

  it("recursively flattens when only the 'all' scope contains multiple modules", {
    decorators <- list(all = list(teal_transform_module(), teal_transform_module()), plot = teal_transform_module())
    dec_out <- select_decorators(decorators, "plot")
    checkmate::expect_list(dec_out, types = "teal_transform_module", len = 3L)
  })

  it("recursively flattens multiple 'all' decorators even if the specific scope is missing", {
    decorators <- list(all = list(teal_transform_module(), teal_transform_module()), plot = teal_transform_module())
    dec_out <- select_decorators(decorators, "other")
    checkmate::expect_list(dec_out, types = "teal_transform_module", len = 2L)
  })
})
