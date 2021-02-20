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
      installed size is 23.6Mb
      sub-directories of 1Mb or more:
        help   1.1Mb
        libs  21.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘withr’
      All declared Imports should be used.
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

