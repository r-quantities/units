context("User-defined unit conversion")

test_that("we can convert between units with a user-defined function", {
  expect_error(as_units("apple"))
  expect_error(as_units("orange"))
  install_symbolic_unit("apple")
  install_symbolic_unit("orange")
  apples <- 2 * as_units("apple")
  oranges <- 3 * as_units("orange")
  expect_error(apples + oranges)
  
  install_conversion_constant("orange", "apple", 2) # one orange is worth two apples
  expect_equal(apples + oranges, (2 + 2*3) * as_units("apple"))
  expect_equal(oranges + apples, (3 + 2/2) * as_units("orange"))
  expect_equal(oranges + apples, set_units(apples + oranges, oranges, mode = "standard"))
  expect_equal(apples + oranges, set_units(apples + oranges, apples, mode = "standard"))
  
  #install_conversion_constant("orange", "apple", 2, 1) # but you always have to add one
  #expect_equal(apples + oranges, (2 + 2*3 + 1) * make_unit("apple"))
  #expect_equal(oranges + apples, (3 + (2 - 1)/2) * make_unit("orange"))
  #expect_equal(oranges + apples, set_units(apples + oranges, oranges))
  
  # now just checking that we get different results with a different fruit
  expect_error(as_units("banana"))
  install_symbolic_unit("banana")
  bananas <- 6 * as_units("banana")
  expect_error(apples + bananas)
  expect_error(bananas + apples)
  
  install_conversion_constant("apple", "banana", 1/3) # one apple only gives you a third banana
  expect_equal(bananas + 3 * apples, (6 + 3 * 2 / 3) * as_units("banana"))
})

test_that("we can simplify via user-defined units", {
  install_symbolic_unit("apple")
  install_symbolic_unit("orange")
  
  apples <- 4 * as_units("apple")
  oranges <- 2 * as_units("orange")
  install_conversion_constant("orange", "apple", 2) # one orange is worth two apples
  
  expect_equal(apples / oranges, set_units(1))
  expect_equal(oranges / apples, set_units(1))
})
