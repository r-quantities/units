# Inside the group generic functions we do have .Generic even if the diagnostics
# think we do not.
# !diagnostics suppress=.Generic

#' @export
Summary.units = function(..., na.rm = FALSE) {
  OK <- switch(.Generic, "sum" = , "min" = , "max" = , "range" = TRUE, FALSE)
  if (!OK)
    stop(paste("Summary operation", .Generic, "not allowed"))
  
  args <- list(...)
  u <- units(args[[1]])
  if (.convert_to_first_arg(args))
    do.call(.Generic, c(args, na.rm = na.rm))
  else structure(NextMethod(), units = u, class = "units")
}

#' @export
print.units = function (x, ...) { # nocov start
  if (is.array(x) || length(x) > 1L) {
    cat("Units: ", as.character(attr(x, "units")), "\n", sep = "")
    x <- unclass(x)
    attr(x, "units") <- NULL
    NextMethod()
  } else {
    cat(format(x, ...), "\n", sep="")
    invisible(x)
  }
} # nocov end

#' @export
mean.units = function(x, ...) {
  .as.units(NextMethod(), units(x))
}

#' @export
weighted.mean.units = mean.units

#' @export
median.units = mean.units

#' @export
quantile.units = function(x, ...) {
  .as.units(quantile(unclass(x), ...), units(x))
}

#' @export
format.units = function(x, ...) {
  paste(NextMethod(), units(x))
}

#' @export
summary.units = function(object, ...) { 
  summary(unclass(object), ...)
}
