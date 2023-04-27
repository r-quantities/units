# gtfs2gps

<details>

* Version: 2.1-0
* GitHub: https://github.com/ipeaGIT/gtfs2gps
* Source code: https://github.com/cran/gtfs2gps
* Date/Publication: 2022-08-16 18:00:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::revdep_details(, "gtfs2gps")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `actual`:   <NA>
      `expected`: TRUE
      ── Failure ('test_gtfs2gps.R:61:5'): gtfs2gps ──────────────────────────────────
      `my_dim` not equal to 126272.
      1/1 mismatches
      [1] 126078 - 126272 == -194
      ── Failure ('test_gtfs2gps.R:69:5'): gtfs2gps ──────────────────────────────────
      poa_gps$trip_number[126272] == 88 is not TRUE
      
      `actual`:   <NA>
      `expected`: TRUE
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 119 ]
      Error: Test failures
      Execution halted
    ```

# vein

<details>

* Version: 1.0.0
* GitHub: https://github.com/atmoschem/vein
* Source code: https://github.com/cran/vein
* Date/Publication: 2023-02-23 00:00:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::revdep_details(, "vein")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `ef_china(...)` threw an error with unexpected message.
      Expected match: "U.?"
      Actual message: "argument is of length zero"
      Backtrace:
          ▆
       1. ├─testthat::expect_error(...) at test-ef_china.R:385:2
       2. │ └─testthat:::quasi_capture(...)
       3. │   ├─testthat (local) .capture(...)
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. └─vein::ef_china(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 714 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 49 marked UTF-8 strings
    ```

