# PKbioanalysis (0.5.0)

* GitHub: <https://github.com/OmarAshkar/PKbioanalysis>
* Email: <mailto:omar.i.elashkar@gmail.com>
* GitHub mirror: <https://github.com/cran/PKbioanalysis>

Run `revdepcheck::revdep_details(, "PKbioanalysis")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
        25. │                                   │ ├─base::tryCatch(...)
        26. │                                   │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
        27. │                                   │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        28. │                                   │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
        29. │                                   │ └─base::withCallingHandlers(...)
        30. │                                   └─duckdb:::rapi_startup(dbdir, readonly, configsexp, environment_scan)
        31. ├─duckdb (local) `<fn>`("rapi_startup", "{\"exception_type\":\"IO\",\"exception_message\":\"Could not set lock on file \\\"/root/.local/share/R/PKbioanalysis/samples.db\\\": Conflicting lock is held in /usr/lib/R/bin/exec/R (PID 23086) by user root. See also https://duckdb.org/docs/stable/connect/concurrency\",\"errno\":\"11\"}")
        32. │ └─rlang::abort(error_parts, class = "duckdb_error", !!!fields)
        33. │   └─rlang:::signal_abort(cnd, .file)
        34. │     └─base::signalCondition(cnd)
        35. └─rlang (local) `<fn>`(`<dckdb_rr>`)
        36.   └─handlers[[1L]](cnd)
        37.     └─duckdb:::rethrow_error_from_rapi(e, call)
        38.       └─rlang::abort(msg, call = call)
       Execution halted
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘htmlwidgets’
       All declared Imports should be used.
     ```

# transfR (1.1.4)

* Email: <mailto:alban.delavenne@inrae.fr>
* GitHub mirror: <https://github.com/cran/transfR>

Run `revdepcheck::revdep_details(, "transfR")` for more info

## Newly broken

*   checking running R code from vignettes ...
     ```
       ‘V01_get_started.Rmd’ using ‘UTF-8’... OK
       ‘V02_inputs_preparation_stars.Rmd’ using ‘UTF-8’... failed
       ‘V03_inputs_preparation_whitebox.Rmd’ using ‘UTF-8’... OK
      ERROR
     Errors in running code in vignettes:
     when running code in ‘V02_inputs_preparation_stars.Rmd’
       ...
     
     > library(units)
     udunits database from /usr/share/xml/udunits/udunits2.xml
     
     > Q <- read.table(file.path(wd, "discharge.txt"), header = TRUE, 
     +     sep = ";", colClasses = c("character", rep("numeric", 6)))
     
       When sourcing ‘V02_inputs_preparation_stars.R’:
     Error: scan() expected 'a real', got '18.577[m^3/s]'
     Execution halted
     ```

