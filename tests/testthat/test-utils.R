test_that("check_pkg_quietly works", {
  expect_error(check_pkg_quietly("my_random_package_1234567890", "my message"), regexp = "my message")
})
