test_that("join_key throws error with invalid keys arguments", {

  # invalid types
  expect_error(join_key("d1", "d2", keys = NULL))
  expect_error(join_key("d1", "d2", keys = 1:10))

  # not fully named
  expect_error(join_key("d1", "d2", keys = c("X" = "A", "B")))
  keys <- c("A", "C" = "B")
  names(keys)[1] <- ""
  expect_error(join_key("d1", "d2", keys))

  # duplicates in names or values
  expect_error(join_key("d1", "d2", keys = c("A" = "A", "A" = "B")))
  expect_error(join_key("d1", "d2", keys = c("C" = "A", "D" = "A")))

  # names(keys)!= keys if datasets are the same
  expect_error(join_key("d1", "d1", keys = c("B" = "A", "A" = "B")))
})

test_that("join_key throws error with invalid dataset arguments", {

  # missing
  expect_error(join_key("d1", as.character(NA), keys = c("A" = "B", "C" = "D")))
  # invalid type
  expect_error(join_key("d1", 5, keys = c("A" = "B", "C" = "D")))
  # invalid length
  expect_error(join_key("d1", c("d1", "d2"), keys = c("A" = "B", "C" = "D")))
})


test_that("join_key does not throw error with valid arguments", {

  # keys of length 0
  expect_silent(join_key("d1", "d2", keys = character(0)))
  # keys of length 1
  expect_silent(join_key("d1", "d2", keys = c("A" = "B")))
  # keys of length > 1
  expect_silent(join_key("d1", "d2", keys = c("A" = "B", "C" = "D")))
  #dataset_1 and dataset_2 can be the same if keys match
  expect_silent(join_key("d1", "d1", keys = c("A" = "A", "B" = "B")))
})


test_that("cannot set join_keys with incompatible keys", {

  # different keys
  expect_error(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D", "E" = "F"))
    )
  )

  expect_error(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d1", "d2", keys = character(0))
    )
  )

  expect_error(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d2", "d1", keys = character(0))
    )
  )

  expect_error(
    join_keys(
      join_key("d1", "d2", keys = character(0)),
      join_key("d2", "d1", keys = c("A" = "B", "C" = "D"))
    )
  )

  expect_error(
    join_keys(
      join_key("d1", "d2", keys = c("a" = "B", "C" = "D")),
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D"))
    )
  )


})

test_that("can create join_keys with compatible information", {

  # different datasets
  expect_silent(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d1", "d3", keys = c("A" = "B", "C" = "D"))
    )
  )

  # same keys
  expect_silent(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D"))
    )
  )

  # reordering keys still matches
  expect_silent(
    join_keys(
      join_key("d1", "d2", keys = c("A" = "B", "C" = "D")),
      join_key("d1", "d2", keys = c("C" = "D", "A" = "B"))
    )
  )

  # can match with empty
  expect_silent(
    join_keys(
      join_key("d1", "d2", keys = character(0)),
      join_key("d1", "d2", keys = character(0))
    )
  )

  expect_silent(
    join_keys(
      join_key("d2", "d1", keys = character(0)),
      join_key("d2", "d1", keys = character(0))
    )
  )

  # swapping dataset order still matches
  expect_silent(
    join_keys(
      join_key("d2", "d1", keys = c("B" = "A", "D" = "C")),
      join_key("d1", "d2", keys = c("C" = "D", "A" = "B"))
    )
  )

})


test_that("cannot create JoinKeys with invalid arguments", {

  # not using join_key
  expect_error(join_keys("d1", "d2", "A"))
  # key sets with the same pair of datasets but different values
  expect_error(join_keys(join_key("d1", "d2", "A"), join_key("d2", "d1", "B")))
  expect_error(join_keys(join_key("d1", "d2", c("A" = "X")), join_key("d2", "d1", c("A" = "X"))))
})

test_that("can create JoinKeys with valid arguments", {
  # no keys
  expect_silent(join_keys())
  # list of keys
  expect_silent(join_keys(join_key("d1", "d2", "A"), join_key("d2", "d3", "B")))
  # single key out of list
  expect_silent(join_keys(join_key("d1", "d2", "A")))
  # key sets with the same pair of datasets and the same values
  expect_silent(join_keys(join_key("d1", "d2", c("A" = "C")), join_key("d2", "d1", c("C" = "A"))))
  expect_silent(join_keys(join_key("d1", "d2", "X"), join_key("d2", "d1", "X")))
})


test_that("cannot set keys in JoinKeys if they have already been set", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  expect_error(my_keys$set(join_key("d1", "d3", "A")))
})


test_that("creating join keys with d1 -> d2 also creates the key d2 - > d1", {
  my_keys <- join_keys(join_key("d1", "d2", c("A" = "C")))
  expect_equal(my_keys$get("d2", "d1"), c("C" = "A"))
})


test_that("can get all keys for a given dataset", {
  my_keys <- join_keys(
    join_key("d1", "d2", c("A" = "C")),
    join_key("d1", "d3", c("A" = "B", "S" = "T")),
    join_key("d2", "d3", c("C" = "U", "L" = "M"))
  )
  expect_equal(my_keys$get(dataset_1 = "d1"), list(d2 = c("A" = "C"), d3 = c("A" = "B", "S" = "T")))
  expect_equal(my_keys$get(dataset_2 = "d1"), list(d2 = c("A" = "C"), d3 = c("A" = "B", "S" = "T")))
  expect_equal(my_keys$get(dataset_1 = "d3"), list(d1 = c("B" = "A", "T" = "S"), d2 = c("U" = "C", "M" = "L")))
})


test_that("can get all keys from JoinKeys", {
  my_keys <- join_keys(
    join_key("d1", "d2", c("A" = "C")),
    join_key("d1", "d3", c("A" = "B", "S" = "T")),
    join_key("d2", "d3", c("C" = "U", "L" = "M"))
  )

  all_keys <- my_keys$get()
  expect_equal(names(all_keys), c("d1", "d2", "d3"))
  expect_equal(my_keys$get(dataset_1 = "d1"), all_keys[["d1"]])

})

test_that("join_key with unamed keys vector creates a JoinKeys with the same column names for both datasets ", {
  test_keys <- join_keys(join_key("d1", "d2", keys = c("A", "B")))
  expect_equal(unname(test_keys$get("d1", "d2")), names(test_keys$get("d1", "d2")))
})


test_that("if no keys between pair of datasets then getting them returns character(0)", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  expect_equal(my_keys$get("d1", "d3"), character(0))
  expect_equal(my_keys$get("d1", "d4"), character(0))
})

test_that("can mutate existing keys", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  my_keys$mutate("d1", "d2", c("X" = "Y"))
  expect_equal(my_keys$get("d1", "d2"),  c("X" = "Y"))
})

test_that("mutating non-existing keys adds them", {
  my_keys <- join_keys(join_key("d1", "d2", "A"))
  my_keys$mutate("d2", "d3", c("X" = "Y"))
  expect_equal(my_keys$get("d3", "d2"),  c("Y" = "X"))
})

test_that("can remove keys by setting them to character(0)", {
  my_keys <- join_keys(join_key("d1", "d2", "A"), join_key("d3", "d4", c("A" = "B", "C" = "D")))
  my_keys$mutate("d1", "d2", character(0))
  expect_equal(my_keys$get("d1", "d2"), character(0))
})

testthat::test_that("JoinKeys$split method returns empty list when object itself is empty", {
  x <- JoinKeys$new()
  testthat::expect_equal(x$split(), list())
})

testthat::test_that("JoinKeys$split method works in general", {
  x <- JoinKeys$new()
  x$set(
    list(
      join_key("A", "B", c("a" = "b")),
      join_key("A", "C", c("a" = "c", "aa" = "cc")),
      join_key("Z", "Y", c("z" = "y"))
    )
  )
  res <- x$split()
  testthat::expect_true(is(res, "list"))
  testthat::expect_equal(length(res), 5)
  testthat::expect_equal(names(res), c("A", "B", "C", "Z", "Y"))
  testthat::expect_true(all(vapply(res, function(x) is(x, "JoinKeys"), logical(1))))

  testthat::expect_equal(names(res$A$get()), c("A", "B", "C"))
  testthat::expect_equal(names(res$B$get()), c("B", "A"))
  testthat::expect_equal(names(res$C$get()), c("C", "A"))
  testthat::expect_equal(names(res$Z$get()), c("Z", "Y"))
  testthat::expect_equal(names(res$Y$get()), c("Y", "Z"))
})
