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

test_that("ids that udunits reads as another unit are refused", {
  in_m <- function(x) as.numeric(set_units(set_units(1, x, mode = "standard"), m))

  # udunits reads an id by name (whatever its case), then by symbol, then
  # after stripping a prefix, so each of these would redefine a database unit
  expect_error(install_unit(name = "m", def = "1 mile"), "already maps")
  expect_error(install_unit("km", "1 mile"), "already maps")
  expect_error(install_unit(name = "kilometer", def = "1 mile"), "already maps")
  expect_error(install_unit(name = "KG", def = "2 kg"), "shadow \"kg\"")
  expect_error(install_unit(name = "KILOM", def = "1 mile"), "shadow \"kilom\"")
  expect_equal(in_m("m"), 1)
  expect_equal(in_m("km"), 1000)
  expect_equal(as.numeric(set_units(set_units(1, kg), g)), 1000)

  # ... or would be unreachable, shadowed by a database name
  expect_error(install_unit("gal", "1 gallon"), "already maps")

  # long names are checked against installed symbols
  install_unit("zzlongsymbolxyz")
  expect_error(install_unit(name = "ZZLONGSYMBOLXYZ", def = "2 zzlongsymbolxyz"),
               "shadow")
  remove_unit("zzlongsymbolxyz")

  # same unit, whatever the route: accepted as aliases
  expect_silent(install_unit("zzkm", "1000 m", "KILOGRAMME_ZZ"))
  expect_equal(in_m("zzkm"), 1000)
  remove_unit("zzkm", "KILOGRAMME_ZZ")

  # an id must be read back as the new unit: "zz1" is read as zz^1
  expect_error(install_unit(c("zzok", "zz1")), "does not read it as this unit")
  expect_error(as_units("zzok"))
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
  # whatever the route by which udunits reads them
  expect_warning(remove_unit("km"), "left unchanged: 'km'")
  expect_warning(remove_unit(name = "kilometer"), "left unchanged")
  expect_warning(remove_unit(name = "m"), "left unchanged")
  expect_warning(remove_unit("gal"), "left unchanged")
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

test_that("install_unit() and remove_unit() check their arguments", {
  expect_error(install_unit(), "at least one")
  expect_error(remove_unit(), "at least one")
  expect_error(install_unit("zz%"), "percentage")

  install_unit("zzdimless", "unitless")
  expect_equal(units(set_units(set_units(1, zzdimless), 1)), units(set_units(1, 1)))
  remove_unit("zzdimless")
})

test_that("NA and empty symbols and names are refused", {
  expect_error(install_unit(NA_character_), "symbol cannot be NA")
  expect_error(install_unit(name = NA_character_), "name cannot be NA")
  expect_error(install_unit("  "), "symbol cannot be empty")
  expect_error(remove_unit(name = ""), "name cannot be empty")
})

test_that("removing a unit's own symbol hands it to its oldest alias", {
  install_unit("zzp")
  install_unit("zzzq", "zzp") # older, but sorts last
  install_unit("zzaq", "zzp")
  remove_unit("zzp")
  expect_equal(ut_symbol("zzaq"), "zzzq")
  remove_unit(c("zzzq", "zzaq"))
  expect_error(as_units("zzaq"))
})

test_that("non-ASCII symbols work, and invalid strings are refused", {
  skip_if_not(l10n_info()[["UTF-8"]])

  install_unit("zzµx")
  expect_equal(ut_symbol("zzµx"), "zzµx")
  remove_unit("zzµx")
  expect_error(as_units("zzµx"))

  # "\xff" alone passes udunits' own UTF-8 check
  for (bad in c("zz\xff", "zz\xc3a")) {
    expect_error(install_unit(c("zzok", bad), name = "zzokname"), "valid strings")
    expect_error(as_units("zzok"))
    expect_error(remove_unit(name = bad), "valid strings")
  }
})

test_that("override = TRUE redefines database ids, and remove_unit() restores them", {
  conv <- function(from, to) ud_convert(1, from, to)
  expect_error(install_unit("zzx", override = NA))

  # a database name, redefined as a name: case variants are the same name
  expect_message(install_unit(name = "gal", def = "1 gallon", override = TRUE),
                 "redefined name \"gal\"")
  expect_equal(conv("gal", "L"), 3.785412, tolerance = 1e-6)
  expect_equal(conv("Gal", "L"), 3.785412, tolerance = 1e-6)
  expect_equal(conv("kgal", "L"), 3785.412, tolerance = 1e-6)
  remove_unit(name = "gal")
  expect_equal(conv("gal", "cm/s^2"), 1)
  # "microN" is the name "micron" too, not micro-newton
  expect_message(install_unit(name = "micron", def = "2 m", override = TRUE))
  expect_equal(conv("microN", "m"), 2)
  remove_unit(name = "micron")
  expect_equal(conv("micron", "um"), 1)

  # ... or as a symbol, which sets the name aside so that it does not win
  expect_message(install_unit("gal", "1 gallon", override = TRUE),
                 "redefined symbol \"gal\"")
  expect_equal(conv("gal", "L"), 3.785412, tolerance = 1e-6)
  remove_unit("gal")
  expect_equal(conv("gal", "cm/s^2"), 1)

  # the own symbol of a unit is set aside too, and restored
  expect_message(install_unit("m", "1 mile", override = TRUE), "was meter")
  expect_equal(conv("m", "ft"), 5280)
  expect_equal(ut_symbol("meter"), character(0))
  expect_equal(conv("meter", "ft"), 3.28084, tolerance = 1e-6)
  remove_unit("m")
  expect_equal(conv("m", "ft"), 3.28084, tolerance = 1e-6)
  expect_equal(ut_symbol("meter"), "m")
  # ... and its own name
  expect_message(install_unit(name = "meter", def = "1 mile", override = TRUE),
                 "was meter")
  expect_equal(ut_name("m"), character(0))
  remove_unit(name = "meter")
  expect_equal(ut_name("m"), "meter")

  # a prefixed meaning needs nothing set aside
  expect_message(install_unit("km", "1 mile", override = TRUE), "was 1000 meter")
  expect_equal(conv("km", "m"), 1609.344)
  remove_unit("km")
  expect_equal(conv("km", "m"), 1000)

  # no conflict: a normal install
  expect_silent(install_unit("zznew", "2 m", override = TRUE))
  remove_unit("zznew")
})

test_that("an id overridden as symbol and name is restored in any removal order", {
  conv <- function(from, to) ud_convert(1, from, to)
  restored <- function() {
    expect_equal(conv("gal", "cm/s^2"), 1)
    expect_equal(conv("Gal", "cm/s^2"), 1)
    expect_equal(conv("gallon", "L"), 3.785412, tolerance = 1e-6)
  }
  install <- function() {
    expect_message(install_unit("gal", "1 gallon", name = "gal", override = TRUE),
                   "redefined")
    expect_equal(conv("gal", "L"), 3.785412, tolerance = 1e-6)
  }

  install(); remove_unit(name = "gal"); remove_unit("gal"); restored()
  install(); remove_unit("gal"); remove_unit(name = "gal"); restored()
  install(); remove_unit("gal", name = "gal"); restored()

  # different ids of the same unit
  expect_message(install_unit(c("gal", "galn"), "1 gallon",
                              name = c("gal", "galname"), override = TRUE))
  expect_equal(conv("galname", "L"), 3.785412, tolerance = 1e-6)
  remove_unit(c("gal", "galn"), c("gal", "galname"))
  restored()

  # one kind alone still restores
  expect_message(install_unit("gal", "1 gallon", override = TRUE))
  remove_unit("gal"); restored()
  expect_message(install_unit(name = "gal", def = "1 gallon", override = TRUE))
  remove_unit(name = "gal"); restored()
})

test_that("override = TRUE still refuses what it cannot undo or did not ask for", {
  conv <- function(from, to) ud_convert(1, from, to)

  # other ids that a new name would shadow
  expect_error(install_unit(name = "KG", def = "2 kg", override = TRUE), "shadow")
  expect_equal(conv("kg", "g"), 1000)

  # ids installed by install_unit()
  install_unit("zzfoo")
  expect_error(install_unit("zzfoo", "2 m", override = TRUE), "remove it first")
  remove_unit("zzfoo")

  # a failure after setting mappings aside puts them back
  expect_error(install_unit(c("gal", "zz1"), "1 gallon", override = TRUE),
               "does not read it as this unit")
  expect_equal(conv("gal", "cm/s^2"), 1)

  # and remove_unit() points to it
  expect_warning(remove_unit("km"), "override = TRUE")
})
