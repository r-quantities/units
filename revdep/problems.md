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

# santoku

<details>

* Version: 0.9.1
* GitHub: https://github.com/hughjonesd/santoku
* Source code: https://github.com/cran/santoku
* Date/Publication: 2023-03-08 13:10:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::revdep_details(, "santoku")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │   │ │ └─base::force(code)
        6. │   │ ├─base::withCallingHandlers(...)
        7. │   │ └─base::withVisible(code)
        8. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        9. └─santoku::chop(x, br, extend = TRUE)
       10.   └─santoku (local) breaks(x, extend, left, close_end)
       11.     └─santoku:::create_extended_breaks(breaks, x, extend, left, close_end)
       12.       └─santoku:::extend_and_close(...)
       13.         └─santoku:::extend_endpoint_left(breaks, x, extend)
       14.           └─santoku:::create_breaks(breaks, c(TRUE, left))
       15.             └─base::stopifnot(all(obj == sort(obj)))
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 10501 ]
      Error: Test failures
      Execution halted
    ```

