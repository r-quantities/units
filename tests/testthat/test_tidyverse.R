test_that("pillar methods are available for units objects", {
  skip_if_not_installed("pillar")

  x = set_units(1:3, km)
  m = c(x, set_units(4:6, g), allow_mixed = TRUE)

  expect_equal(unclass(pillar::type_sum(x)), "[km]")
  expect_s3_class(pillar::type_sum(x), "type_sum_units")
  expect_equal(pillar::type_sum(m), "mixed_units")

  expect_snapshot({
    pillar::pillar(x[1])
    pillar::pillar(m[1])
  })
})

test_that("units have coercion methods", {
  skip_if_not_installed("vctrs", "0.3.1")

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
})

test_that("can combine units vectors", {
  skip_if_not_installed("vctrs", "0.3.1")
  skip_if_not_installed("dplyr", "1.0.0")

  x <- set_units(1:3, "cm")
  y <- set_units(4, "m")

  exp = set_units(c(1, 2, 3, 400), "cm")
  expect_identical(vctrs::vec_c(x, y), exp)

  # Recursive case
  df1 = dplyr::tibble(x = dplyr::tibble(x = x))
  df2 = dplyr::tibble(x = dplyr::tibble(x = y))
  df_exp = dplyr::tibble(x = dplyr::tibble(x = exp))
  expect_identical(vctrs::vec_c(df1, df2), df_exp)
})

test_that("can slice units vectors", {
  skip_if_not_installed("vctrs", "0.3.1")
  skip_if_not_installed("dplyr", "1.0.0")

  x = set_units(1:3, "cm")
  exp = list(set_units(1L, "cm"), set_units(2L, "cm"), set_units(3L, "cm"))
  expect_identical(vctrs::vec_chop(x), exp)

  # Recursive case
  df = dplyr::tibble(dplyr::tibble(x = x))
  exp = list(
    dplyr::tibble(x = set_units(1L, "cm")),
    dplyr::tibble(x = set_units(2L, "cm")),
    dplyr::tibble(x = set_units(3L, "cm"))
  )
  expect_identical(vctrs::vec_chop(df), exp)
})

test_that("split-apply-combine with dplyr and base agree", {
  skip_if_not_installed("vctrs", "0.3.1")
  skip_if_not_installed("dplyr", "1.0.0")

  `%>%` <- dplyr::`%>%`
  iris2 <- iris
  for (i in 1:4)
    units(iris2[,i]) <- "cm"

  out <- iris2 %>%
    dplyr::group_by(Species) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), mean))

  # Transform to list of lists
  out <- vctrs::vec_chop(out[2:5]) %>%
    stats::setNames(out$Species) %>%
    lapply(as.list)

  exp <- lapply(split(iris2[1:4], iris2$Species), lapply, mean)
  expect_equal(out, exp)
})

test_that("storage mode is uniformly set to double (see #324)", {
  skip_if_not_installed("vctrs", "0.3.1")

  x <- set_units(1:10, cm)
  br <- set_units(2:4, `in`)
  vctrs::vec_cast_common(x, br)
})
