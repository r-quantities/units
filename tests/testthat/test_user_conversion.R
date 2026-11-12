test_that("we can convert between units with a user-defined function", {
  expect_error(as_units("apple"))
  expect_error(as_units("orange"))
  install_unit("orange")
  oranges <- 3 * as_units("orange")
  expect_error(apples + oranges) # obviously

  install_unit("apple", "orange / 2") # one orange is worth two apples
  apples <- 2 * as_units("apple")
  expect_equal(apples + oranges, (2 + 2*3) * as_units("apple"))
  expect_equal(oranges + apples, (3 + 2/2) * as_units("orange"))
  expect_equal(oranges + apples, set_units(apples + oranges, units(oranges), mode = "standard"))
  expect_equal(apples + oranges, set_units(apples + oranges, units(apples), mode = "standard"))

  # now just checking that we get different results with a different fruit
  expect_error(as_units("banana"))
  expect_error(apples + bananas) # obviously
  expect_error(bananas + apples) # obviously

  install_unit("banana", "3 apple") # one apple is worth three bananas
  bananas <- 6 * as_units("banana")
  expect_equal(bananas + 3 * apples, (6 + 3 * 2 / 3) * as_units("banana"))

  # check dimensionless
  install_unit("person", "1")
  persons <- set_units(3, person)
  expect_true(persons == set_units(3, 1))

  # restore
  remove_unit(c("orange", "apple", "banana", "person"))
})

test_that("we can simplify via user-defined units", {
  install_unit("orange")
  install_unit("apple", "orange / 2") # one orange is worth two apples
  apples <- 4 * as_units("apple")
  oranges <- 2 * as_units("orange")
  expect_equal(apples / oranges, set_units(1))
  expect_equal(oranges / apples, set_units(1))

  # restore
  remove_unit(c("orange", "apple"))
})

test_that("removing units works", {
  expect_silent(remove_unit("foo"))
  expect_silent(install_unit("foo"))
  expect_error(install_unit("foo"))
  expect_silent(remove_unit("foo"))
})

test_that("a refused install_unit() maps nothing", {
  # the unit already has a symbol
  expect_error(install_unit("zzdeg", "1 degree"), "already maps")
  expect_error(as_units("zzdeg"))
  expect_silent(remove_unit("zzdeg"))
  expect_equal(as.character(units(as_units("degree"))), "\u00b0")

  # the unit already has a name
  expect_error(install_unit(name = "zzdegree", def = "1 degree"), "already maps")
  expect_error(as_units("zzdegree"))

  # the name is refused: the symbol is not mapped either
  install_unit("zzone", name = "zzone_name")
  expect_error(install_unit("zztwo", "3 m", "zzone_name"), "already maps")
  expect_error(as_units("zztwo"))

  # a later symbol is refused: the earlier one is not mapped either
  expect_error(install_unit(c("zzthree", "zzone"), "3 m"), "already maps")
  expect_error(as_units("zzthree"))

  # restore
  remove_unit("zzone", "zzone_name")
})
