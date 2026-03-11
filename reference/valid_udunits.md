# Get information about valid units

These functions require the xml2 package, and return data frames with
complete information about pre-defined units from UDUNITS2. Inspect this
data frames to determine what inputs are accepted by `as_units` (and the
other functions it powers: `as_units`, `set_units` , `units<-`).

## Usage

``` r
valid_udunits(quiet = FALSE)

valid_udunits_prefixes(quiet = FALSE)
```

## Arguments

- quiet:

  logical, defaults `TRUE` to give a message about the location of the
  udunits database being read.

## Value

a data frame with columns `symbol` , `symbol_aliases` , `name_singular`
, `name_singular_aliases` , `name_plural` , or `name_plural_aliases` ,
`def` , `definition` , `comment` , `dimensionless` and `source_xml`

## Details

Any entry listed under `symbol` , `symbol_aliases` , ` name_singular` ,
`name_singular_aliases` , `name_plural` , or `name_plural_aliases` is
valid. Additionally, any entry under `symbol` or `symbol_aliases` may
can also contain a valid prefix, as specified by
`valid_udunits_prefixes()` .

Note, this is primarily intended for interactive use, the exact format
of the returned data frames may change in the future.

## Examples

``` r
if (requireNamespace("xml2", quietly = TRUE)) {
  valid_udunits()
  valid_udunits_prefixes()
  if(interactive())
    View(valid_udunits())
}
#> udunits database from /usr/share/xml/udunits/udunits2.xml
#> udunits database from /usr/share/xml/udunits/udunits2.xml
```
