context("User-defined unit conversion")

test_that("we can convert between units with a user-defined function", {
  apples <- 2 * make_unit("apple")
  oranges <- 3 * make_unit("orange")
  expect_error(apples + oranges)
  
  install_conversion_constant("orange", "apple", 2) # one orange is worth two apples
  expect_equal(apples + oranges, (2 + 2*3) * make_unit("apple"))
  expect_equal(oranges + apples, (3 + 2/2) * make_unit("orange"))

  install_conversion_constant("orange", "apple", 2, 1) # but you always have to add one
  expect_equal(apples + oranges, (2 + 2*3 + 1) * make_unit("apple"))
  expect_equal(oranges + apples, (3 + (2 - 1)/2) * make_unit("orange"))
})