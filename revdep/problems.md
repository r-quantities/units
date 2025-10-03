# epocakir

<details>

* Version: 0.9.9
* GitHub: https://github.com/alwinw/epocakir
* Source code: https://github.com/cran/epocakir
* Date/Publication: 2023-01-06 15:30:06 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::revdep_details(, "epocakir")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘epocakir-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GFR_staging
    > ### Title: GFR Staging
    > ### Aliases: GFR_staging GFR_staging.data.frame GFR_staging.units
    > ###   GFR_staging.numeric
    > 
    > ### ** Examples
    > 
    > df <- tibble::tibble(
    +   eGFR = units::set_units(c(-1, NA, 100, 70, 50, 35, 20, 10), "mL/min/1.73m2")
    + )
    > 
    > GFR_staging(df, "eGFR")
    Error: cannot convert mL/min/1.73m^2 into mL/min
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─epocakir::GFR_staging(df, "eGFR") at test-ckd.R:378:3
        2. └─epocakir:::GFR_staging.data.frame(df, "eGFR")
        3.   ├─epocakir::GFR_staging(.data[[rlang::as_name(rlang::enquo(GFR))]])
        4.   └─epocakir:::GFR_staging.units(.data[[rlang::as_name(rlang::enquo(GFR))]])
        5.     ├─epocakir::GFR_staging(as_metric(GFR = GFR, value_only = TRUE))
        6.     └─epocakir::as_metric(GFR = GFR, value_only = TRUE)
        7.       ├─units::set_units(meas, conversion$metric_units, mode = "standard")
        8.       └─units:::set_units.units(meas, conversion$metric_units, mode = "standard")
        9.         ├─base::`units<-`(`*tmp*`, value = as_units(value, ...))
       10.         └─units:::`units<-.units`(`*tmp*`, value = as_units(value, ...))
      
      [ FAIL 1 | WARN 9 | SKIP 0 | PASS 598 ]
      Error: Test failures
      Execution halted
    ```

