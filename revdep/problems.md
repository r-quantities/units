# constants

<details>

* Version: 1.0.0
* GitHub: https://github.com/r-quantities/constants
* Source code: https://github.com/cran/constants
* Date/Publication: 2020-11-11 07:40:12 UTC
* Number of recursive dependencies: 38

Run `revdep_details(, "constants")` for more info

</details>

## Newly broken

*   checking whether package ‘constants’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'units::remove_symbolic_unit' is deprecated.
      Warning: 'units::install_conversion_constant' is deprecated.
    See ‘/home/iucar/Documents/repos/units/revdep/checks/constants/new/constants.Rcheck/00install.out’ for details.
    ```

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: 'units::remove_symbolic_unit' is deprecated.
      Warning: 'units::install_conversion_constant' is deprecated.
    Deprecated functions may be defunct as soon as of the next release of
    R.
    See ?Deprecated.
    ```

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: 'units::remove_symbolic_unit' is deprecated.
    Use 'remove_unit' instead.
    See help("Deprecated")
    Warning: 'units::install_conversion_constant' is deprecated.
    Use 'install_unit' instead.
    See help("Deprecated")
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

# EmissV

<details>

* Version: 0.665.3.0
* GitHub: https://github.com/atmoschem/EmissV
* Source code: https://github.com/cran/EmissV
* Date/Publication: 2020-10-08 08:00:03 UTC
* Number of recursive dependencies: 67

Run `revdep_details(, "EmissV")` for more info

</details>

## Newly broken

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: 'units::install_conversion_constant' is deprecated.
    Deprecated functions may be defunct as soon as of the next release of
    R.
    See ?Deprecated.
    ```

# eplusr

<details>

* Version: 0.14.0
* GitHub: https://github.com/hongyuanjia/eplusr
* Source code: https://github.com/cran/eplusr
* Date/Publication: 2021-01-07 07:00:38 UTC
* Number of recursive dependencies: 102

Run `revdep_details(, "eplusr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Prefixes will automatically work with any user-defined unit.
      Backtrace:
          █
       1. ├─testthat::expect_equal(...) test-units.R:4:4
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─units::set_units(1, "person")
       5. └─units:::set_units.numeric(1, "person")
       6.   ├─units::as_units(value, ...)
       7.   └─units:::as_units.character(value, ...)
       8.     └─units:::as_units.call(expr, check_is_valid = check_is_valid)
      
      [ FAIL 1 | WARN 0 | SKIP 74 | PASS 2062 ]
      Error: Test failures
      Execution halted
    ```

# ggforce

<details>

* Version: 0.3.2
* GitHub: https://github.com/thomasp85/ggforce
* Source code: https://github.com/cran/ggforce
* Date/Publication: 2020-06-23 09:40:02 UTC
* Number of recursive dependencies: 82

Run `revdep_details(, "ggforce")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggforce-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scale_unit
    > ### Title: Position scales for units data
    > ### Aliases: scale_x_unit scale_y_unit scale_type.units
    > 
    > ### ** Examples
    > 
    > library(units)
    udunits database from /usr/share/udunits/udunits2.xml
    > gallon <- as_units('gallon')
    > mtcars$consumption <- mtcars$mpg * with(ud_units, mi / gallon)
    Error in with(ud_units, mi/gallon) : object 'ud_units' not found
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 23.7Mb
      sub-directories of 1Mb or more:
        help   1.1Mb
        libs  21.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘withr’
      All declared Imports should be used.
    ```

# magclass

<details>

* Version: 5.15.6
* GitHub: https://github.com/pik-piam/magclass
* Source code: https://github.com/cran/magclass
* Date/Publication: 2020-12-14 18:10:08 UTC
* Number of recursive dependencies: 61

Run `revdep_details(, "magclass")` for more info

</details>

## Newly broken

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: 'units::install_conversion_constant' is deprecated.
      Warning: 'units::install_conversion_constant' is deprecated.
      Warning: 'units::install_conversion_constant' is deprecated.
      Warning: 'units::install_symbolic_unit' is deprecated.
      Warning: 'units::install_conversion_constant' is deprecated.
      Warning: 'units::install_conversion_constant' is deprecated.
      Warning: 'units::install_symbolic_unit' is deprecated.
      Warning: 'units::install_symbolic_unit' is deprecated.
    ...
      Warning: 'units::install_symbolic_unit' is deprecated.
      Warning: 'units::install_symbolic_unit' is deprecated.
      Warning: 'units::install_conversion_constant' is deprecated.
      Warning: 'units::install_conversion_constant' is deprecated.
      Warning: 'units::install_conversion_constant' is deprecated.
      Warning: 'units::install_conversion_constant' is deprecated.
      Warning: 'units::install_symbolic_unit' is deprecated.
    Deprecated functions may be defunct as soon as of the next release of
    R.
    See ?Deprecated.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘udunits2’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘udunits2’
    ```

# quantities

<details>

* Version: 0.1.5
* GitHub: https://github.com/r-quantities/quantities
* Source code: https://github.com/cran/quantities
* Date/Publication: 2020-06-14 05:20:02 UTC
* Number of recursive dependencies: 55

Run `revdep_details(, "quantities")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘quantities-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Extract.quantities
    > ### Title: Extract or Replace Parts of an Object
    > ### Aliases: Extract.quantities [.quantities [[.quantities [<-.quantities
    > ###   [[<-.quantities
    > 
    > ### ** Examples
    > 
    > x <- set_quantities(1:3, m/s, 0.1)
    > y <- set_quantities(4:6, m/s, 0.2)
    > (z <- rbind(x, y))
    Error in data.frame(val = as.numeric(x), from = I(units(x)), to = value,  : 
      'list' object cannot be coerced to type 'double'
    Calls: rbind -> rbind -> <Anonymous> -> data.frame
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─quantities:::expect_quantities(...) test-utils.R:30:2
        2. │ └─testthat::expect_equal(class(x), c("quantities", "units", "errors")) helper-quantities.R:3:2
        3. │   └─testthat::quasi_label(enquo(object), label, arg = "object")
        4. │     └─rlang::eval_bare(expr, quo_get_env(quo))
        5. └─base::cbind(x, y, x, y)
        6.   └─quantities:::cbind(deparse.level, ...)
        7.     └─getS3method("set_units", "mixed_units")(dots, as.character(u))
        8.       ├─base::data.frame(...)
        9.       ├─base::I(units(x))
       10.       │ └─base::structure(x, class = unique(c("AsIs", oldClass(x))))
       11.       └─base::units(x)
      
      [ FAIL 4 | WARN 11 | SKIP 0 | PASS 457 ]
      Error: Test failures
      Execution halted
    ```

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'units.Rd':
      ‘[units]{ud_units}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# traitdataform

<details>

* Version: 0.6.1
* GitHub: https://github.com/ecologicaltraitdata/traitdataform
* Source code: https://github.com/cran/traitdataform
* Date/Publication: 2020-11-11 07:40:02 UTC
* Number of recursive dependencies: 99

Run `revdep_details(, "traitdataform")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘units::parse_unit’
    ```

# yamlet

<details>

* Version: 0.6.10
* GitHub: NA
* Source code: https://github.com/cran/yamlet
* Date/Publication: 2021-02-20 08:00:02 UTC
* Number of recursive dependencies: 78

Run `revdep_details(, "yamlet")` for more info

</details>

## Newly broken

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: 'install_symbolic_unit' is deprecated.
    Deprecated functions may be defunct as soon as of the next release of
    R.
    See ?Deprecated.
    ```

