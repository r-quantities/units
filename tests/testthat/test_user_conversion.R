context("User-defined unit conversion")

test_that("we can convert between units with a user-defined function", {
  expect_error(as_units("apple"))
  expect_error(as_units("orange"))
  #install_symbolic_unit("apple") -> needs to be defined by install_conversion_constant
  install_symbolic_unit("orange")
  oranges <- 3 * as_units("orange")
  expect_error(apples + oranges) # obviously

  install_conversion_constant("orange", "apple", 2) # one orange is worth two apples
  apples <- 2 * as_units("apple")
  expect_equal(apples + oranges, (2 + 2*3) * as_units("apple"))
  expect_equal(oranges + apples, (3 + 2/2) * as_units("orange"))
  # FIXME: expect_equal(oranges + apples, set_units(apples + oranges, oranges, mode = "standard"))
  # FIXME: expect_equal(apples + oranges, set_units(apples + oranges, apples, mode = "standard"))

  #install_conversion_constant("orange", "apple", 2, 1) # but you always have to add one
  #expect_equal(apples + oranges, (2 + 2*3 + 1) * as_units("apple"))
  #expect_equal(oranges + apples, (3 + (2 - 1)/2) * as_units("orange"))
  #expect_equal(oranges + apples, set_units(apples + oranges, oranges))

  # now just checking that we get different results with a different fruit
  expect_error(as_units("banana"))
  #install_symbolic_unit("banana") -> need to be done by install_user_conversion
  expect_error(apples + bananas) # obviously
  expect_error(bananas + apples) # obviously

  install_conversion_constant("apple", "banana", 1/3) # one apple only gives you a third banana
  bananas <- 6 * as_units("banana")
  expect_equal(bananas + 3 * apples, (6 + 3 * 2 / 3) * as_units("banana"))

  install_symbolic_unit("aaa")
  install_conversion_offset("aaa", "bbb", 2) # bbb is aaa + 2
  expect_warning(install_symbolic_unit("aaa"), "is already a valid unit")

  # restore
  remove_symbolic_unit("orange")
  remove_symbolic_unit("apple")
  remove_symbolic_unit("banana")
  remove_symbolic_unit("aaa")
  remove_symbolic_unit("bbb")
})

test_that("we can simplify via user-defined units", {
  install_symbolic_unit("orange")
  install_conversion_constant("orange", "apple", 2) # one orange is worth two apples
  apples <- 4 * as_units("apple")
  oranges <- 2 * as_units("orange")
  expect_equal(apples / oranges, set_units(1))
  expect_equal(oranges / apples, set_units(1))

  # restore
  remove_symbolic_unit("orange")
  remove_symbolic_unit("apple")
})

test_that("removing units works", {
  expect_error(remove_symbolic_unit("foo"))
  expect_silent(install_symbolic_unit("foo"))
  expect_silent(remove_symbolic_unit("foo"))
  expect_error(remove_symbolic_unit("foo"))
})

test_that("new units' format is checked for possible issues", {
  wrong_formats <- c(
    " 2asdf", "asdf2 ",
    "as+df", "as-df", "as*df", "as/df", "as^df",
    "as df")
  for (i in wrong_formats) {
    expect_error(install_symbolic_unit(i))
    expect_error(install_conversion_constant(i, "m", 2))
    expect_error(install_conversion_constant("m", i, 2))
    expect_error(install_conversion_offset(i, "m", 2))
    expect_error(install_conversion_offset("m", i, 2))
  }
})
