# EmissV

<details>

* Version: 0.665.5.2
* GitHub: https://github.com/atmoschem/EmissV
* Source code: https://github.com/cran/EmissV
* Date/Publication: 2021-03-31 11:10:02 UTC
* Number of recursive dependencies: 68

Run `revdep_details(, "EmissV")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          █
       1. ├─testthat::expect_equal(...) test-emission.R:62:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─base::nrow(...)
       5. ├─EmissV::emission(...)
       6. │ └─base::suppressWarnings(units::install_symbolic_unit("MOL"))
       7. │   └─base::withCallingHandlers(...)
       8. └─base::.handleSimpleError(...)
       9.   └─base:::h(simpleError(msg, call))
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported objects:
      ‘units::install_symbolic_unit’ ‘units::remove_symbolic_unit’
    ```

# eplusr

<details>

* Version: 0.15.0
* GitHub: https://github.com/hongyuanjia/eplusr
* Source code: https://github.com/cran/eplusr
* Date/Publication: 2021-11-17 17:00:05 UTC
* Number of recursive dependencies: 62

Run `revdep_details(, "eplusr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported objects:
      ‘units::install_conversion_constant’ ‘units::install_symbolic_unit’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        R   5.6Mb
    ```

# fgdr

<details>

* Version: 1.1.0
* GitHub: https://github.com/uribo/fgdr
* Source code: https://github.com/cran/fgdr
* Date/Publication: 2020-09-30 07:00:02 UTC
* Number of recursive dependencies: 122

Run `revdep_details(, "fgdr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(fgdr)
      > 
      > test_check("fgdr")
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-read_fgd_dem.R:43:3): Successed on dummies ────────────────────
      unique(res$value) not equal to c(-9999L, NA_real_).
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      target is units, current is numeric
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 50 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# gtfs2gps

<details>

* Version: 1.5-4
* GitHub: https://github.com/ipeaGIT/gtfs2gps
* Source code: https://github.com/cran/gtfs2gps
* Date/Publication: 2021-09-06 08:00:07 UTC
* Number of recursive dependencies: 91

Run `revdep_details(, "gtfs2gps")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      target is units, current is numeric
      ── Failure (test_gtfs2gps.R:63:5): gtfs2gps ────────────────────────────────────
      sum(poa_gps$dist) not equal to 516072.
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      target is units, current is numeric
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 186 ]
      Error: Test failures
      Execution halted
    ```

# jpmesh

<details>

* Version: 2.0.2
* GitHub: https://github.com/uribo/jpmesh
* Source code: https://github.com/cran/jpmesh
* Date/Publication: 2021-06-25 11:10:07 UTC
* Number of recursive dependencies: 113

Run `revdep_details(, "jpmesh")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      NULL
      NULL
      NULL
      NULL
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (5)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-neighborhood.R:93:3): corners ─────────────────────────────────
      ... %>% sf::st_area() not equivalent to 9455968.
      target is units, current is numeric
      
      [ FAIL 1 | WARN 0 | SKIP 5 | PASS 175 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 176 marked UTF-8 strings
    ```

# photosynthesis

<details>

* Version: 2.0.1
* GitHub: https://github.com/cdmuir/photosynthesis
* Source code: https://github.com/cran/photosynthesis
* Date/Publication: 2021-07-01 04:30:02 UTC
* Number of recursive dependencies: 100

Run `revdep_details(, "photosynthesis")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      target is units, current is numeric
      ── Failure (test-unitless.R:120:3): unitless values match unit-ed values ───────
      `Wt1` not equal to `Wt2`.
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      target is units, current is numeric
      
      [ FAIL 6 | WARN 0 | SKIP 0 | PASS 149 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc    3.4Mb
        help   1.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘future’
      All declared Imports should be used.
    ```

# RWmisc

<details>

* Version: 0.1.1
* GitHub: https://github.com/jayrobwilliams/RWmisc
* Source code: https://github.com/cran/RWmisc
* Date/Publication: 2021-06-20 10:00:11 UTC
* Number of recursive dependencies: 84

Run `revdep_details(, "RWmisc")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      target is units, current is numeric
      ── Failure (test-pointpolydist.R:19:3): point.poly.dist works with projected CRS ──
      point.poly.dist(points_t, projectUTM(poly_t)) not equal to 100201.5.
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      target is units, current is numeric
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 19 ]
      Error: Test failures
      Execution halted
    ```

# sf

<details>

* Version: 1.0-5
* GitHub: https://github.com/r-spatial/sf
* Source code: https://github.com/cran/sf
* Date/Publication: 2021-12-17 13:00:02 UTC
* Number of recursive dependencies: 151

Run `revdep_details(, "sf")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    Missing or unexported object: ‘units::install_conversion_constant’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 24.7Mb
      sub-directories of 1Mb or more:
        doc      1.7Mb
        libs    18.1Mb
        sqlite   1.5Mb
    ```

# vein

<details>

* Version: 0.9.4
* GitHub: https://github.com/atmoschem/vein
* Source code: https://github.com/cran/vein
* Date/Publication: 2021-10-08 08:00:02 UTC
* Number of recursive dependencies: 87

Run `revdep_details(, "vein")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      target is units, current is numeric
      ── Failure (test-Vehicles.R:8:3): Vehicles works ───────────────────────────────
      Vehicles(as.numeric(ef_cetesb("CO_0km", "PC_G")))[[1]] not equal to 0.1612112.
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      target is units, current is numeric
      
      [ FAIL 29 | WARN 0 | SKIP 0 | PASS 686 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 49 marked UTF-8 strings
    ```

