test_that("split_address handles normal case", {
  building <- list(ADDRESS = "Route de Courgenay 20")
  building <- split_address(building)
  expect_equal(building$DEINR, "20")
  expect_equal(building$STRNAME, "Route de Courgenay")
})

test_that("split_address handles street number with letter", {
  building <- list(ADDRESS = "Route de Courgenay 20a")
  building <- split_address(building)
  expect_equal(building$DEINR, "20a")
  expect_equal(building$STRNAME, "Route de Courgenay")
})

test_that("split_address handles multiple street numbers", {
  building <- list(ADDRESS = "Route de Courgenay 20a, 20b, 301")
  building <- split_address(building)
  expect_equal(building$DEINR, "20a, 20b, 301")
  expect_equal(building$STRNAME, "Route de Courgenay")
})

test_that("split_address handles no street number", {
  building <- list(ADDRESS = "Route de Courgenay ")
  building <- split_address(building)
  expect_equal(is.na(building$DEINR), TRUE)
  expect_equal(building$STRNAME, "Route de Courgenay")
})

test_that("split_address handles no spaces between numbers", {
  building <- list(ADDRESS = "Route de Courgenay 20,20b,301")
  building <- split_address(building)
  expect_equal(building$DEINR, "20, 20b, 301")
  expect_equal(building$STRNAME, "Route de Courgenay")
})

test_that("split_address handles spaces in address and between numbers", {
  building <- list(ADDRESS = "  Route de Courgenay    20   ,  20b,301")
  building <- split_address(building)
  expect_equal(building$DEINR, "20, 20b, 301")
  expect_equal(building$STRNAME, "Route de Courgenay")
})

test_that("split_address handles number in street name", {
  building <- list(ADDRESS = "Rue du 23 Juin 10, 25")
  building <- split_address(building)
  expect_equal(building$DEINR, "10, 25")
  expect_equal(building$STRNAME, "Rue du 23 Juin")
})

test_that("split_address handles empty address", {
  building <- list(ADDRESS = "  ")
  building <- split_address(building)
  expect_equal(is.na(building$DEINR), TRUE)
  expect_equal(is.na(building$STRNAME), TRUE)
})