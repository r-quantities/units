# quantities

<details>

* Version: 0.2.0
* GitHub: https://github.com/r-quantities/quantities
* Source code: https://github.com/cran/quantities
* Date/Publication: 2022-12-05 08:30:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::revdep_details(, "quantities")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • plot/ggplot2-x-scale.svg
      • plot/ggplot2-x.svg
      • plot/ggplot2-y-scale.svg
      • plot/ggplot2-y.svg
      • plot/plot-x-drop-errors.svg
      • plot/plot-x-drop-units.svg
      • plot/plot-x.svg
      • plot/plot-xy-drop-errors-x.svg
      • plot/plot-xy-drop-errors-y.svg
      • plot/plot-xy-drop-quantities-y.svg
      • plot/plot-xy-drop-units-x.svg
      • plot/plot-xy-drop-units-y.svg
      • plot/plot-xy.svg
      Error: Test failures
      Execution halted
    ```

# tealeaves

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/tealeaves
* Date/Publication: 2022-07-20 14:30:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::revdep_details(, "tealeaves")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tealeaves-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Ar
    > ### Title: Ar: Archimedes number
    > ### Aliases: Ar
    > 
    > ### ** Examples
    > 
    > cs <- make_constants()
    > ep <- make_enviropar()
    Error in enviro_par(.) : 
      .x$r >= set_units(0) & .x$r <= set_units(1) is not TRUE
    Calls: make_enviropar -> %<>% -> enviro_par -> stopifnot
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       3. └─tealeaves::leaf_par(.)
       4.   └─base::stopifnot(.x$abs_l >= set_units(0) & .x$abs_l <= set_units(1))
      ── Error ('test-unitless.R:7:3'): unitless values match unit-ed values ─────────
      Error in `leaf_par(list(abs_l = set_units(runif(1)), abs_s = set_units(runif(1)), 
          g_sw = set_units(runif(1, 0, 10), "umol/m^2/s/Pa"), g_uw = set_units(runif(1), 
              "umol/m^2/s/Pa"), leafsize = set_units(runif(1), "m"), 
          logit_sr = set_units(runif(1, -10, 10))))`: .x$abs_l >= set_units(0) & .x$abs_l <= set_units(1) is not TRUE
      Backtrace:
          ▆
       1. └─tealeaves::leaf_par(...) at test-unitless.R:7:2
       2.   └─base::stopifnot(.x$abs_l >= set_units(0) & .x$abs_l <= set_units(1))
      
      [ FAIL 9 | WARN 0 | SKIP 0 | PASS 45 ]
      Error: Test failures
      Execution halted
    ```

