# Load a unit system

Load an XML database containing a unit system compatible with UDUNITS2.

## Usage

``` r
load_units_xml(path = default_units_xml())
```

## Arguments

- path:

  a path to a valid unit system in XML format.

## Details

A unit system comprises a root `<unit-system>` and a number of children
defining prefixes (`<prefix>`) or units (`<unit>`). See the contents of

`system.file("share/udunits", package="units")`

for examples.

## Examples

``` r
# load a new unit system
load_units_xml(system.file("share/udunits/udunits2-base.xml", package="units"))
if (FALSE) { # \dontrun{
set_units(1, rad) # doesn'twork
} # }

# reload the default unit system
load_units_xml()
set_units(1, rad) # works again
#> 1 [rad]
```
