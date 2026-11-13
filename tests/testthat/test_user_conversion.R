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

ut_symbol <- function(x) R_ut_get_symbol(R_ut_parse(x))
ut_name <- function(x) R_ut_get_name(R_ut_parse(x))

test_that("a refused install_unit() maps nothing", {
  # a symbol or name that belongs to another unit
  expect_silent(install_unit("zzm", "m", "zzmeter"))
  expect_error(install_unit(c("zza", "s"), "3 m"), "already maps")
  expect_error(as_units("zza"))
  expect_error(install_unit("zzb", "3 m", "second"), "already maps")
  expect_error(as_units("zzb"))
  expect_equal(ut_symbol("s"), "s")
  expect_equal(ut_symbol("m"), "m")
  expect_silent(remove_unit("zzm", "zzmeter"))

  # installed twice, with a different unit the second time
  install_unit("zzc")
  expect_error(install_unit(c("zzd", "zzc")), "already maps")
  expect_error(as_units("zzd"))
  remove_unit("zzc")
})

test_that("symbols and names of existing units are installed as aliases", {
  install_unit("zzdeg", "1 degree", "zzdegree")
  expect_equal(set_units(set_units(180, zzdeg), rad), set_units(pi, rad))
  expect_equal(set_units(set_units(1, zzdegree), degree), set_units(1, degree))
  expect_equal(ut_symbol("degree"), "°")
  expect_equal(ut_symbol("m"), "m")

  remove_unit("zzdeg", "zzdegree")
  expect_error(as_units("zzdeg"))
  expect_error(as_units("zzdegree"))
  expect_equal(ut_symbol("degree"), "°")

  # installing an existing symbol again changes nothing
  install_unit("fn", "2 week", "fortnight")
  expect_silent(install_unit("fn", "2 week", "fortnight"))
  remove_unit("fn", "fortnight")
  expect_error(as_units("fn"))
})

test_that("removing an alias keeps the unit's own symbol and name", {
  install_unit("zzu", name = "zzunit")
  install_unit(c("zzu", "zzalias"), "zzu", c("zzunit", "zzalias_name"))
  remove_unit("zzalias", "zzalias_name")
  expect_equal(ut_symbol("zzu"), "zzu")
  expect_equal(ut_name("zzu"), "zzunit")
  expect_error(as_units("zzalias"))

  # removing the unit's own symbol hands it to a remaining alias
  install_unit("zzalias", "zzu")
  remove_unit("zzu")
  expect_equal(ut_symbol("zzalias"), "zzalias")
  remove_unit("zzalias", "zzunit")
  expect_error(as_units("zzalias"))
  expect_error(as_units("zzunit"))
})

test_that("remove_unit() leaves units it did not install", {
  expect_warning(remove_unit("m", "second"), "left unchanged")
  expect_equal(set_units(set_units(1, km), m), set_units(1000, m))
  expect_equal(ut_symbol("m"), "m")
  expect_equal(ut_name("s"), "second")
})

test_that("user-defined symbols are available in ASCII", {
  install_unit("zzcell")
  on.exit(remove_unit("zzcell"))
  on.exit(ud_set_encoding(if (l10n_info()[["UTF-8"]]) "utf8"
                          else if (l10n_info()[["Latin-1"]]) "latin1"
                          else "ascii"), add = TRUE)
  ud_set_encoding("ascii")
  expect_equal(ut_symbol("zzcell"), "zzcell")
})
