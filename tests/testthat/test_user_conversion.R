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
  install_unit("person", "unitless")
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
