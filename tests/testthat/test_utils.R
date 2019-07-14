context("utils")

test_that("is in operator", {
  # if both are vectors then it behaves like normal %in%
  expect_identical(c("a", "b") %is_in% c("a", "b", "c"),
                   c("a", "b") %in% c("a", "b", "c"))
  expect_identical(c("a", "b", "d") %is_in% c("a", "b", "c"),
               c("a", "b", "d") %in% c("a", "b", "c"))


  # if one is a list it looks for same vector
  expect_true(all(c("a", "b", "c") %is_in% list(c("a", "b", "c"))))
  expect_false(all(c("a", "b") %is_in% list("a", "b", "c")))

  vec1 <- c("a", "b")
  list1 <- list(c("a", "b"), c("b", "d"))
  list2 <- list(c("a"), c("b", "d"))
  list3 <- list(c("a", "b"), c("b", "d"), c("a", "d"))

  expect_true(all(vec1 %is_in% list1))
  expect_false(all(vec1 %is_in% list2))
  expect_true(all(list1 %is_in% list3))

})
