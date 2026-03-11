# Changelog

## version 1.0-1

CRAN release: 2026-03-11

- Add internal workaround for udunits2 bug with parsing of units that
  contain “per”, e.g. “percent” or “person”;
  [\#414](https://github.com/r-quantities/units/issues/414)

- Update minimum Rcpp version;
  [\#421](https://github.com/r-quantities/units/issues/421)

- Fix `print` methods for `units` and `mixed_units`, which now correctly
  return invisibly the object being printed;
  [\#422](https://github.com/r-quantities/units/issues/422)

- Wrap all pointers to avoid memory leaks derived from errors.

- Check for exception propagation in udunits2;
  [\#423](https://github.com/r-quantities/units/issues/423)

- Fix [`ud_convert()`](../reference/udunits2.md) for empty input
  vectors; [\#428](https://github.com/r-quantities/units/issues/428)

## version 1.0-0

CRAN release: 2025-10-09

- Breaking change: a new tokenizer fixes longstanding issues with
  parsing complex unit expressions, but may break existing code that
  relied on the previous (buggy) behavior. The major change is that now
  numbers are consistently treated as prefixes, so that units like
  `ml / min / 1.73m^2` used in physiology are now correctly parsed as
  `ml / (min * 1.73 * m^2)`. See [`?as_units`](../reference/units.md)
  for details; [\#416](https://github.com/r-quantities/units/issues/416)
  addressing [\#221](https://github.com/r-quantities/units/issues/221),
  [\#383](https://github.com/r-quantities/units/issues/383)

- Printing: follow NIST recommendations. In particular, numerator and
  denominator are separated by a single slash, and a parenthesis is used
  when the denominator contains several symbols (see example above);
  [\#86](https://github.com/r-quantities/units/issues/86)

- Vectorize `ud_*()` helpers;
  [\#405](https://github.com/r-quantities/units/issues/405) addressing
  [\#404](https://github.com/r-quantities/units/issues/404)

- Loading units no longer initializes the RNG state;
  [\#409](https://github.com/r-quantities/units/issues/409)

- Fix scale training in `ggplot2` scales;
  [\#412](https://github.com/r-quantities/units/issues/412)

- Add `scale_{type}_units()` scales for additional continuous aesthetics
  (colour, fill, alpha, size, linewidth);
  [\#369](https://github.com/r-quantities/units/issues/369)

- Implement `matrixOps.units`, with support for `%*%` (R \>= 4.3.0);
  [\#226](https://github.com/r-quantities/units/issues/226)

- New [`convert_to_base()`](../reference/convert_to_base.md) implements
  conversion to base units;
  [\#132](https://github.com/r-quantities/units/issues/132)
  [@jamarav](https://github.com/jamarav)

## version 0.8-7

CRAN release: 2025-03-11

- Deep copy of [`ud_convert()`](../reference/udunits2.md) input to avoid
  side effects;
  [\#403](https://github.com/r-quantities/units/issues/403)

- Set C++17 standard for old versions of R;
  [\#402](https://github.com/r-quantities/units/issues/402)

## version 0.8-6

CRAN release: 2025-03-07

- Add methods for `cbind` and `rbind`; fixes
  [\#311](https://github.com/r-quantities/units/issues/311)

- Performance improvements in `data.frame` methods; suggested in
  [\#361](https://github.com/r-quantities/units/issues/361)
  [@grcatlin](https://github.com/grcatlin)

- Fix `weighted.mean.units` for unitless objects;
  [\#363](https://github.com/r-quantities/units/issues/363)

- Fix incorrect use of [`round()`](https://rdrr.io/r/base/Round.html) in
  `%%` and `%/%` methods;
  [\#365](https://github.com/r-quantities/units/issues/365)
  [@UchidaMizuki](https://github.com/UchidaMizuki)

- Fix `ggplot2` deprecation warnings;
  [\#367](https://github.com/r-quantities/units/issues/367)

- Fix [`hist()`](https://rdrr.io/r/graphics/hist.html) error;
  [\#368](https://github.com/r-quantities/units/issues/368)

- Add support for
  [`lims()`](https://ggplot2.tidyverse.org/reference/lims.html) in
  `ggplot2` scales;
  [\#370](https://github.com/r-quantities/units/issues/370)

- Fix simplification of inverse units;
  [\#378](https://github.com/r-quantities/units/issues/378)

- Replace call to `Rf_error()` with `Rcpp::stop()`; RcppCore/Rcpp#1247

- Fix UBs in the C++ glue code;
  [\#380](https://github.com/r-quantities/units/issues/380)

- Add support for `brew` path discovery in macOS;
  [\#384](https://github.com/r-quantities/units/issues/384)

- Several performance improvements;
  [\#387](https://github.com/r-quantities/units/issues/387),
  [\#388](https://github.com/r-quantities/units/issues/388),
  [\#393](https://github.com/r-quantities/units/issues/393),
  [\#400](https://github.com/r-quantities/units/issues/400) addressing
  [\#386](https://github.com/r-quantities/units/issues/386),
  [\#389](https://github.com/r-quantities/units/issues/389)

- Improve [`keep_units()`](../reference/keep_units.md) helper for more
  general usage scenarios;
  [\#394](https://github.com/r-quantities/units/issues/394)
  [@d-morrison](https://github.com/d-morrison) addressing
  [\#392](https://github.com/r-quantities/units/issues/392)

- Add [`ud_convert()`](../reference/udunits2.md) to convert units of a
  vector; [\#399](https://github.com/r-quantities/units/issues/399)
  [@dlebauer](https://github.com/dlebauer) addressing
  [\#398](https://github.com/r-quantities/units/issues/398)

- Fix `scale_units` for upcoming version of ggplot2;
  [\#401](https://github.com/r-quantities/units/issues/401)

## version 0.8-5

CRAN release: 2023-11-28

- avoid -Wformat-security warning on CRAN

## version 0.8-4

CRAN release: 2023-09-13

- Identical unit division and multiplication will now respect
  `units_options(simplify = FALSE)` reverting a change from
  [\#310](https://github.com/r-quantities/units/issues/310);
  [\#355](https://github.com/r-quantities/units/issues/355)
  [@billdenney](https://github.com/billdenney)

- Fix `scale_units` when both `unit` and `trans` are specified;
  [\#357](https://github.com/r-quantities/units/issues/357)

## version 0.8-3

CRAN release: 2023-08-10

- Remove tolerance from comparisons with logical operators, restoring
  behavior changed in previous release;
  [\#353](https://github.com/r-quantities/units/issues/353) addressing
  [\#351](https://github.com/r-quantities/units/issues/351)

## version 0.8-2

CRAN release: 2023-04-27

- Names are preserved when doing unit conversions;
  [\#305](https://github.com/r-quantities/units/issues/305)
  [@billdenney](https://github.com/billdenney)

- Identical units will always divide (`/`) and allow integer division
  (`%/%`). And, inverse units will always be able to multiply;
  [\#310](https://github.com/r-quantities/units/issues/310)
  [@billdenney](https://github.com/billdenney)

- Compare units via `ud_compare()`, fixing inconsistent results for
  aliases and symbols;
  [\#339](https://github.com/r-quantities/units/issues/339),
  [\#346](https://github.com/r-quantities/units/issues/346),
  [\#347](https://github.com/r-quantities/units/issues/347)

- Fix `units<-()` to treat an empty unit the same as `NULL`;
  [\#332](https://github.com/r-quantities/units/issues/332)

- New [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  method for `mixed_units`;
  [\#309](https://github.com/r-quantities/units/issues/309)

- Use short paths for database loading on Windows to fix UTF-16 issues;
  [\#342](https://github.com/r-quantities/units/issues/342)

- Add example in the docs about reversing a ggplot2 units scale;
  [\#343](https://github.com/r-quantities/units/issues/343)

- Force storage mode to double;
  [\#344](https://github.com/r-quantities/units/issues/344) addressing
  [\#324](https://github.com/r-quantities/units/issues/324)

- Fix units in transformed axis;
  [\#323](https://github.com/r-quantities/units/issues/323)

- Consider unitless as radians in trigonometric functions;
  [\#328](https://github.com/r-quantities/units/issues/328)

## version 0.8-1

CRAN release: 2022-12-10

- fix `%/%` and `%%` if arguments have different units;
  [\#313](https://github.com/r-quantities/units/issues/313)

- fix multiplier parsing for `exp(log(x))` operations;
  [\#321](https://github.com/r-quantities/units/issues/321)

- fix specification of secondary axes with `scale_units`;
  [\#326](https://github.com/r-quantities/units/issues/326)

## version 0.8-0

CRAN release: 2022-02-04

- enhance unit mapping for newly installed units;
  [\#290](https://github.com/r-quantities/units/issues/290)

- remove deprecations: `install_symbolic_unit`, `remove_symbolic_unit`,
  `install_conversion_constant`, `install_conversion_offset`;
  [\#290](https://github.com/r-quantities/units/issues/290)

- fix multipliers for round trip log-exp operations;
  [\#292](https://github.com/r-quantities/units/issues/292)

- integrate `ggplot2` scales (previously in the `ggforce` package) to
  automatically print axes with units;
  [\#294](https://github.com/r-quantities/units/issues/294) addressing
  [\#164](https://github.com/r-quantities/units/issues/164)

- fix `all.equal.units` for non-units `current`

- fix zero power;
  [\#285](https://github.com/r-quantities/units/issues/285)

- fix `unique.units` to support arrays and matrices, implement methods
  for `duplicated` and `anyDuplicated`

- fix plot labels with spaces;
  [\#298](https://github.com/r-quantities/units/issues/298) addressing
  [\#297](https://github.com/r-quantities/units/issues/297)

- always add units to labels, including user-provided ones; as part of
  [\#298](https://github.com/r-quantities/units/issues/298)

- new symbols/names with a percentage character are not allowed due to
  an upstream bug;
  [\#289](https://github.com/r-quantities/units/issues/289)

## version 0.7-2

CRAN release: 2021-06-08

- enhance `pillar` integration;
  [\#273](https://github.com/r-quantities/units/issues/273),
  [\#275](https://github.com/r-quantities/units/issues/275),
  [\#280](https://github.com/r-quantities/units/issues/280)
  [@krlmlr](https://github.com/krlmlr)

- new `unique` method for `units` and `mixed_units` objects;
  [\#283](https://github.com/r-quantities/units/issues/283) addressing
  [\#277](https://github.com/r-quantities/units/issues/277)
  [@lewinfox](https://github.com/lewinfox)

## version 0.7-1

CRAN release: 2021-03-16

- allow longer units grouping;
  [\#270](https://github.com/r-quantities/units/issues/270) addressing
  [\#269](https://github.com/r-quantities/units/issues/269)
  [@bart1](https://github.com/bart1)

- fix regression in `set_units` method for `mixed_units` to ensure that
  ordering is preserved;
  [\#272](https://github.com/r-quantities/units/issues/272) addressing
  [\#271](https://github.com/r-quantities/units/issues/271)

## version 0.7-0

CRAN release: 2021-02-25

- add `load_units_xml` to enable database reloading as well as loading
  user-provided unit systems;
  [\#254](https://github.com/r-quantities/units/issues/254) addressing
  [\#243](https://github.com/r-quantities/units/issues/243),
  [\#244](https://github.com/r-quantities/units/issues/244)

- add `install_unit` and `remove_unit` for adding/removing custom
  user-defined symbols or names, with optional mapping to existing
  units; `install_symbolic_unit`, `remove_symbolic_unit`,
  `install_conversion_constant`, `install_conversion_offset` are
  deprecated; [\#261](https://github.com/r-quantities/units/issues/261)
  addressing [\#89](https://github.com/r-quantities/units/issues/89)

- add `keep_units`, a helper to apply functions that do not preserve
  units; [\#255](https://github.com/r-quantities/units/issues/255)
  addressing [\#252](https://github.com/r-quantities/units/issues/252)

- fix `as_units("")`, which is now equivalent to `unitless`;
  [\#199](https://github.com/r-quantities/units/issues/199)

- fix plot axes for `plot.formula` and `plot.data.frame`;
  [\#213](https://github.com/r-quantities/units/issues/213)

- fix arithmetic for powers above 1 and below -1;
  [\#264](https://github.com/r-quantities/units/issues/264)

- improve arithmetic of logarithms;
  [\#249](https://github.com/r-quantities/units/issues/249)

- export `ud_are_convertible`;
  [\#263](https://github.com/r-quantities/units/issues/263) addressing
  [\#258](https://github.com/r-quantities/units/issues/258)
  [@cregouby](https://github.com/cregouby)

- remove deprecations: `as.units`, `as_cf`, `make_unit`, `parse_unit`;
  [\#259](https://github.com/r-quantities/units/issues/259)

- remove deprecated pre-computed `ud_units` database;
  [\#259](https://github.com/r-quantities/units/issues/259)

## version 0.6-7

CRAN release: 2020-06-13

- port `isFALSE` to fix regression in old R versions;
  [\#230](https://github.com/r-quantities/units/issues/230) addressing
  [\#229](https://github.com/r-quantities/units/issues/229)

- fix replacement operation for `units` objects;
  [\#233](https://github.com/r-quantities/units/issues/233) addressing
  [\#232](https://github.com/r-quantities/units/issues/232)

- fix compatibility with dplyr 1.0;
  [\#247](https://github.com/r-quantities/units/issues/247) addressing
  [\#239](https://github.com/r-quantities/units/issues/239)

## version 0.6-6

CRAN release: 2020-03-16

- prettier `str` print for units and mixed units;
  [\#228](https://github.com/r-quantities/units/issues/228) addressing
  [\#227](https://github.com/r-quantities/units/issues/227)

- add compatibility with upcoming tibble v3.0.0;
  [\#225](https://github.com/r-quantities/units/issues/225)

## version 0.6-5

CRAN release: 2019-10-08

- skip test on CRAN to avoid issues with strict latin1 environments

## version 0.6-4

CRAN release: 2019-08-22

- fix support for weights with units in `weighted.mean`;
  [\#205](https://github.com/r-quantities/units/issues/205)

- invalid names for new units now trigger a proper error message;
  [\#209](https://github.com/r-quantities/units/issues/209) addressing
  [\#208](https://github.com/r-quantities/units/issues/208)

- fix issues in strict latin1 environments;
  [\#202](https://github.com/r-quantities/units/issues/202)

## version 0.6-3

CRAN release: 2019-05-03

- improve platform dependent encodings handling;
  [\#183](https://github.com/r-quantities/units/issues/183)

- don’t force `as.numeric` when unnecessary;
  [\#182](https://github.com/r-quantities/units/issues/182) addressing
  [\#181](https://github.com/r-quantities/units/issues/181)

- fix valgrind issues on CRAN and tidy up tests;
  [\#193](https://github.com/r-quantities/units/issues/193) addressing
  [\#192](https://github.com/r-quantities/units/issues/192)

- new method `drop_units` for data frames;
  [\#191](https://github.com/r-quantities/units/issues/191) addressing
  [\#187](https://github.com/r-quantities/units/issues/187)

## version 0.6-2

CRAN release: 2018-12-05

- fix support for logarithms and decibels;
  [\#177](https://github.com/r-quantities/units/issues/177) addressing
  [\#176](https://github.com/r-quantities/units/issues/176)
- add delayed S3 registration mechanism for R \>= 3.6.0

## version 0.6-1

CRAN release: 2018-09-21

- vectors with mixed units are now supported;
  [\#145](https://github.com/r-quantities/units/issues/145)

- `NA` values for units now trigger a proper error message;
  [\#163](https://github.com/r-quantities/units/issues/163)

## version 0.6-0

CRAN release: 2018-06-09

- print units as \[unit\] more consistently, e.g. for single unit and in
  data.frames; [\#132](https://github.com/r-quantities/units/issues/132)

- improve printing of unitless units; provide option to print something
  else than 1; [\#150](https://github.com/r-quantities/units/issues/150)

- fix printing unitless in labels when `negative_power` is `TRUE`;
  [\#133](https://github.com/r-quantities/units/issues/133)

- `install_symbolic_unit` now adds a dimensionless unit, integrated in
  the units system, meaning that prefixes on it work as well;
  [\#71](https://github.com/r-quantities/units/issues/71)

- `install_conversion_constant` and `install_conversion_offset` now
  install a new unit that is a function of an existing udunits unit.;
  [\#71](https://github.com/r-quantities/units/issues/71),
  [\#84](https://github.com/r-quantities/units/issues/84)

- unit simplification can now be user-controlled by `units_options`;
  [\#89](https://github.com/r-quantities/units/issues/89)

- `set_units(15, mg/kg)` is now no longer simplified to 1e-9 unitless;
  [\#89](https://github.com/r-quantities/units/issues/89)

- directly uses the udunits2 C library; drop dependence on R package
  `udunits2`, fixing R package `udunits2` memory leaks;
  [\#135](https://github.com/r-quantities/units/issues/135)

- drops `%*%`, no longer gives warning when loading

## version 0.5-1

CRAN release: 2018-01-08

## version 0.5-0

CRAN release: 2018-01-07

- deal with trigonometric functions for units degree; return units rad
  on inverse trigonometric functions.

- Unit creation has been significantly refactored. `units<-` now accepts
  strings or quoted language objects on the right hand side, powered by
  new S3 methods for `as_units`. All valid unit symbols and unit names
  recognized by package ‘udunits2’ are now accepted. New user facing
  function [`make_units()`](../reference/units.md) (plural s) is also
  provided. See [`?as_units`](../reference/units.md) for details.
  [@t-kalinowski](https://github.com/t-kalinowski)

- new functions [`valid_udunits()`](../reference/valid_udunits.md) and
  [`valid_udunits_prefixes()`](../reference/valid_udunits.md) generate
  tidy dataframes listing all the valid unit names, symbols, and
  prefixes recognized by udunits.
  [@t-kalinowski](https://github.com/t-kalinowski)

- new function `install_symbolic_unit()` for adding custom, user-defined
  units. [@t-kalinowski](https://github.com/t-kalinowski)

- `make_unit` and `parse_unit` (singular unit) have been deprecated,
  please use `as_units` instead.

- `ud_units` is no longer necessary and is soft-deprecated, and may be
  removed in a future release.

- add `%*%` as an S3 generic;
  [\#54](https://github.com/r-quantities/units/issues/54)

- add `%%` and `%/%` to `Ops.units`

- support unary + and - ;
  [\#56](https://github.com/r-quantities/units/issues/56)

- add `seq` method for `units`, converting units to those of the first
  argument

- Deprecate `as.dt` for `as_difftime`, `as.units` for `as_units` and
  `as_cf` for `deparse_unit`

## version 0.4-6

CRAN release: 2017-08-27

- add `all.equal` method for `units`;
  [\#51](https://github.com/r-quantities/units/issues/51)

- add `deparse_unit` to replace `as_cf`

- add calender/time conversions between `udunits` time units like
  `minutes from 1900-0-0`, and R’s `POSIXct` and `Date`

- add `as_units` to replace `as.units`

- rename `as.dt` to `as_difftime`

## version 0.4-5

CRAN release: 2017-06-14

- add support for user-defined unit conversion;
  [\#31](https://github.com/r-quantities/units/issues/31)

- allow for 1/n integer powers, as in `set_units(1:10, m^-2) ^ 0.5`;
  [\#29](https://github.com/r-quantities/units/issues/29)

- properly set log units after log transform;
  [\#33](https://github.com/r-quantities/units/issues/33)

- `sin`, `cos` and `tan` no longer complain when units is `rad`, and
  return `unitless`;
  [\#40](https://github.com/r-quantities/units/issues/40)

- now allow for `set_units(1:3, "°C")` and also
  `set_units(1:3, "degree_Celsius")` by resolving names to symbols
  first; [\#43](https://github.com/r-quantities/units/issues/43)

- `set_units(x)` with `x` numeric sets units to `unitless`;
  [\#41](https://github.com/r-quantities/units/issues/41)

## version 0.4-4

CRAN release: 2017-04-20

- fix a result units bug when multiplying or dividing units vectors of
  different length,
  [\#34](https://github.com/r-quantities/units/issues/34)

- add a `rep` method for `units` vectors

## version 0.4-3

CRAN release: 2017-03-25

- support for `set_units(1:10, m)` which does not require to declare or
  define, `m` (`m` is resolved automatically from `ud_units`)
