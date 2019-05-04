context("Mixed Unit tests")

test_that("mixed units work", {
   (m = c(set_units(1:3, km), set_units(4:6, g), allow_mixed = TRUE))

   # select a subset
   expect_s3_class(m[3:4], "mixed_units")
   expect_s3_class(m[3], "mixed_units")

# select a single units object:
   expect_s3_class(m[[3]], "units")

   m <- set_units(m, c(rep(c("m", "kg"), each = 3)))
   expect_s3_class(m, "mixed_units")
   units(m) = rep(c("mm", "mg"), each = 3)
   expect_s3_class(m, "mixed_units")
   # does the value get recycled?
   expect_s3_class(set_units(m[1:3], "m"), "mixed_units")

   # convert to units:
   expect_s3_class(as_units( m[1:3] ), "units")

# round-trip via units:
   m0 <- mixed_units(as_units(m[1:3]))
   expect_identical(m[1:3], m0)

# Ops using by single unit: needs to be explicitly coerced to mixed_units:
   expect_s3_class(m[1:3] * mixed_units(set_units(1, mm)), "mixed_units")
   expect_s3_class(m[1:3] / mixed_units(set_units(1, mm)), "mixed_units")
   expect_s3_class(m[1:3] + mixed_units(set_units(1, mm)), "mixed_units")
   expect_s3_class(m[1:3] - mixed_units(set_units(1, mm)), "mixed_units")
   expect_is(m[1:3] == mixed_units(set_units(1, mm)), "logical")
   expect_is(m[1:3] != mixed_units(set_units(1, mm)), "logical")
   expect_error(m[1:3] ^ mixed_units(set_units(1, mm)))

# this breaks -- seems to be an s3 limitation:
   expect_error(m[1:3] * set_units(1, mm))

   expect_s3_class(units(m), "mixed_symbolic_units")
   expect_is(format(m), "character")
   expect_is(as.character(units(m)), "character")
   print(m)
   expect_equal(drop_units(m), sapply(m, as.numeric))

   skip_if_not_installed("tibble")
   print(tibble::tibble(m))
   str(m)
   units_options(allow_mixed = TRUE)
   m = c(set_units(1:3, km), set_units(4:6, g))
   units_options(allow_mixed = FALSE)
})
