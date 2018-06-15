# Inside the group generic functions we do have .Generic even if the diagnostics
# think we do not.
# !diagnostics suppress=.Generic

#' @export
Summary.units = function(..., na.rm = FALSE) {
  OK <- switch(.Generic, "sum" = , "min" = , "max" = , "range" = TRUE, FALSE)
  if (! OK)
    stop(paste("Summary operation", .Generic, "not allowed"))
  
  args <- list(...)
  if (length(args) > 1) {
    args <- do.call(c, args) # turns args into a single units vector
    do.call(.Generic, c(list(args), na.rm = na.rm)) # concatenate, convert if necessary, re-call with length 1 arg
  } else
	.as.units(NextMethod(), units(args[[1]]))
}


#' @export
print.units = function (x, ...) { # nocov start
  if (is.array(x) || length(x) > 1L) {
    cat("Units: ", as.character(attr(x, "units")), "\n", sep = "")
    x <- drop_units(x)
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
  setNames(paste(NextMethod(), units(x)), names(x))
}

#' @export
summary.units = function(object, ...) { 
  summary(unclass(object), ...)
}
