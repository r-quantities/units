context("User-defined unit conversion")

test_that("we can convert between units with a user-defined function", {
  apples <- 2 * make_unit("apple", user_defined = TRUE)
  oranges <- 3 * make_unit("orange", user_defined = TRUE)
  expect_error(apples + oranges)
  
  install_conversion_constant("orange", "apple", 2) # one orange is worth two apples
  expect_equal(apples + oranges, (2 + 2*3) * make_unit("apple", user_defined = TRUE))
  expect_equal(oranges + apples, (3 + 2/2) * make_unit("orange", user_defined = TRUE))
  expect_equal(oranges + apples, set_units(apples + oranges, oranges, mode = "units"))
  expect_equal(apples + oranges, set_units(apples + oranges, apples, mode = "units"))
  
  #install_conversion_constant("orange", "apple", 2, 1) # but you always have to add one
  #expect_equal(apples + oranges, (2 + 2*3 + 1) * make_unit("apple"))
  #expect_equal(oranges + apples, (3 + (2 - 1)/2) * make_unit("orange"))
  #expect_equal(oranges + apples, set_units(apples + oranges, oranges))
  
  # now just checking that we get different results with a different fruit
  bananas <- 6 * make_unit("banana", user_defined = TRUE)
  expect_error(apples + bananas)
  expect_error(bananas + apples)
  
  install_conversion_constant("apple", "banana", 1/3) # one apple only gives you a third banana
  expect_equal(bananas + 3 * apples, (6 + 3 * 2 / 3) * make_unit("banana", user_defined = TRUE))
})

test_that("we can simplify via user-defined units", {
  apples <- 4 * make_unit("apple", user_defined = TRUE)
  oranges <- 2 * make_unit("orange", user_defined = TRUE)
  install_conversion_constant("orange", "apple", 2) # one orange is worth two apples
  
  expect_equal(apples / oranges, set_units(1))
  expect_equal(oranges / apples, set_units(1))
})
