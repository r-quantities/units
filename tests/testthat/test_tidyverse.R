
skip_if_not_installed("vctrs")

test_that("units have coercion methods", {
  x = set_units(1:3, "cm")
  y = set_units(4.0, "m")
  z = set_units(10, "celsius")

  expect_error(vctrs::vec_ptype_common(y, x, z), class = "vctrs_error_incompatible_type")
  expect_error(vctrs::vec_cast_common(y, x, z), class = "vctrs_error_incompatible_type")

  expect_identical(vctrs::vec_ptype_common(x, y, x), set_units(double(), "cm"))
  expect_identical(vctrs::vec_ptype_common(x, x), set_units(integer(), "cm"))
  expect_identical(vctrs::vec_ptype_common(y, x, x), set_units(double(), "m"))

  expect_identical(
    vctrs::vec_cast_common(x, y),
    list(set_units(c(1, 2, 3), "cm"), set_units(400, "cm"))
  )
  expect_identical(
    vctrs::vec_cast_common(y, x),
    list(set_units(4, "m"), set_units(c(0.01, 0.02, 0.03), "m"))
  )

  # Casting to integer with fractional cm is lossy
  expect_error(
    vctrs::vec_cast_common(y, x, .to = set_units(0L, "m")),
    class = "vctrs_error_cast_lossy"
  )
})

test_that("can combine units vectors", {
  x <- set_units(1:3, "cm")
  y <- set_units(4, "m")

  exp = set_units(c(1, 2, 3, 400), "cm")
  expect_identical(vctrs::vec_c(x, y), exp)

  # Recursive case
  df1 = tibble::tibble(x = tibble::tibble(x = x))
  df2 = tibble::tibble(x = tibble::tibble(x = y))
  df_exp = tibble::tibble(x = tibble::tibble(x = exp))
  expect_identical(vctrs::vec_c(df1, df2), df_exp)
})
