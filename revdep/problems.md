# divvy

<details>

* Version: 1.0.0
* GitHub: https://github.com/GawainAntell/divvy
* Source code: https://github.com/cran/divvy
* Date/Publication: 2023-10-26 08:20:03 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::revdep_details(, "divvy")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘divvy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rangeSize
    > ### Title: Calculate common metrics of spatial distribution
    > ### Aliases: rangeSize
    > 
    > ### ** Examples
    > 
    > # generate 20 occurrences for a pseudo-species
    ...
    > set.seed(2)
    > x <- rnorm(20, 110.5885, 2)
    > y <- rnorm(20,  44.4280, 1)
    > pts <- cbind(x,y)
    > 
    > rangeSize(pts)
    Error in UseMethod("units") : 
      no applicable method for 'units' applied to an object of class "c('integer', 'numeric')"
    Calls: rangeSize -> cbind -> cbind -> units
    Execution halted
    ```

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Expected `rangeSize(bivAlt[, xyCell], crs = prj)` to run without any conditions.
      i Actually got a <simpleError> with text:
        no applicable method for 'units' applied to an object of class "c('integer', 'numeric')"
      ── Error ('test_calc_eco_variables.R:43:3'): rangeSize and sdSumry accept projected coords ──
      Error in `UseMethod("units")`: no applicable method for 'units' applied to an object of class "c('integer', 'numeric')"
      Backtrace:
          ▆
       1. └─divvy::rangeSize(bivAlt[, xyCell], crs = prj) at test_calc_eco_variables.R:43:3
       2.   └─base::cbind(...)
       3.     └─units (local) cbind(deparse.level, ...)
       4.       └─base::units(dots[[1]])
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 22 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘habitat-rangesize-case-study.Rmd’ using ‘UTF-8’... OK
      ‘subsampling-vignette.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘subsampling-vignette.Rmd’
      ...
    > unique(names(bandLocs))
    [1] "[-50,-30)" "[-10,10)"  "[10,30)"   "[30,50)"   "[50,70)"  
    
    > unsamp <- sdSumry(dat = bivalves, taxVar = "genus", 
    +     collections = "collection_no", xy = xyCell, quotaQ = 0.8, 
    +     quotaN = 100, omitDom =  .... [TRUNCATED] 
    
      When sourcing ‘subsampling-vignette.R’:
    Error: no applicable method for 'units' applied to an object of class "c('integer', 'numeric')"
    Execution halted
    ```

# move2

<details>

* Version: 0.4.2
* GitHub: NA
* Source code: https://github.com/cran/move2
* Date/Publication: 2024-12-18 23:00:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::revdep_details(, "move2")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `expected`: " NA [°]" " 45 [°]" "-90 [°]" " 90 [°]" " NA [°]"
      ── Failure ('test-calculate_properties.R:124:3'): turn angles in -180 180 range ──
      set_units(...) (`actual`) not identical to set_units(c(NA, 45L, -90L, 90L, NA), "degrees") (`expected`).
      
      `actual`:   " NA [°]" " 45 [°]" "270 [°]" " 90 [°]" " NA [°]"
      `expected`: " NA [°]" " 45 [°]" "-90 [°]" " 90 [°]" " NA [°]"
      ── Failure ('test-calculate_properties.R:129:3'): turn angles in -180 180 range ──
      set_units(...) (`actual`) not identical to set_units(c(NA, -45L, 90L, -90L, NA), "degrees") (`expected`).
      
      `actual`:   " NA [°]" "315 [°]" " 90 [°]" "270 [°]" " NA [°]"
      `expected`: " NA [°]" "-45 [°]" " 90 [°]" "-90 [°]" " NA [°]"
      
      [ FAIL 6 | WARN 0 | SKIP 17 | PASS 750 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ...
    ```
      ‘albatross.Rmd’ using ‘UTF-8’... failed
      ‘convert.Rmd’ using ‘UTF-8’... OK
      ‘filtering_tracks.Rmd’ using ‘UTF-8’... failed
      ‘movebank.Rmd’ using ‘UTF-8’... failed
      ‘programming_move2_object.Rmd’ using ‘UTF-8’... failed
      ‘trajectory_analysis.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘albatross.Rmd’
      ...
    ...
    > Sys.setlocale("LC_TIME", "en_US.utf8")
    [1] "en_US.utf8"
    
    > library(move2)
    
    > vulture_data <- movebank_download_study("Turkey vultures in North and South America")
    
      When sourcing ‘trajectory_analysis.R’:
    Error: The 'system' keyring does not exists, create it first!
    Execution halted
    ```

# Rdistance

<details>

* Version: 3.0.0
* GitHub: https://github.com/tmcd82070/Rdistance
* Source code: https://github.com/cran/Rdistance
* Date/Publication: 2023-06-13 07:30:05 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::revdep_details(, "Rdistance")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Rdistance-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.dfunc
    > ### Title: plot.dfunc - Plot method for distance (detection) functions
    > ### Aliases: plot.dfunc
    > ### Keywords: models
    > 
    > ### ** Examples
    > 
    ...
    +    , col="wheat"
    +    , density=30
    +    , angle=c(-45,0,45)
    +    , cex.axis=1.5
    +    , cex.lab=2
    +    , ylab="Probability") 
    Error in UseMethod("units") : 
      no applicable method for 'units' applied to an object of class "logical"
    Calls: plot ... barplot.default -> xyrect -> rect -> rbind -> rbind -> units
    Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) halfnorm.like.Rd:105: Lost braces; missing escapes or markup?
       105 |   {f(x|a,b,c_1,c_2,...,c_k) = f(x|a,b)(1 + c(1) h_i1(x) + c(2) h_i2(x) + ... + c(k) h_ik(x)). }
           |   ^
    checkRd: (-1) secondDeriv.Rd:15: Lost braces
        15 | This must be a function of the form FUN <- function(x, ...){...}
           |                                                            ^
    ```

# stars

<details>

* Version: 0.6-7
* GitHub: https://github.com/r-spatial/stars
* Source code: https://github.com/cran/stars
* Date/Publication: 2024-11-07 20:40:02 UTC
* Number of recursive dependencies: 165

Run `revdepcheck::revdep_details(, "stars")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘aggregate.R’
      Comparing ‘aggregate.Rout’ to ‘aggregate.Rout.save’ ... OK
      Running ‘align.R’
      Comparing ‘align.Rout’ to ‘align.Rout.save’ ... OK
      Running ‘area.R’
      Comparing ‘area.Rout’ to ‘area.Rout.save’ ... OK
      Running ‘crop.R’
      Comparing ‘crop.Rout’ to ‘crop.Rout.save’ ... OK
      Running ‘curvilinear.R’
      Comparing ‘curvilinear.Rout’ to ‘curvilinear.Rout.save’ ... OK
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-cubble.R:32:3'): cubble ──────────────────────────────────────
      all.equal(drop_units(a), a2, check.attributes = FALSE) is not TRUE
      
      `actual` is a character vector ('Component 1: target is matrix, current is units')
      `expected` is a logical vector (TRUE)
      
      [ FAIL 1 | WARN 1 | SKIP 4 | PASS 70 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘starsdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        doc   2.9Mb
        nc    1.7Mb
    ```

# vein

<details>

* Version: 1.1.3
* GitHub: https://github.com/atmoschem/vein
* Source code: https://github.com/cran/vein
* Date/Publication: 2024-05-01 13:50:02 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::revdep_details(, "vein")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Weighted mean =  1.5 
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 701 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-long_to_wide.R:9:3'): long_to_wide works ─────────────────────
      long_to_wide(df)$CO[1] not equal to 1.
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 2, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      target is units, current is numeric
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 701 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 49 marked UTF-8 strings
    ```

