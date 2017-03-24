# Inside the group generic functions we do have .Generic even if the diagnostics
# think we do not.
# !diagnostics suppress=.Generic

#' @export
Summary.units = function(..., na.rm = FALSE) {
  OK <- switch(.Generic, "sum" = , "min" = , "max" = , "range" = TRUE, FALSE)
  if (!OK)
    stop(paste("Summary operation", .Generic, "not allowed"))
  
  args = list(...)
  u = units(args[[1]])
  if (length(args) > 1) {
    for (i in 2:length(args)) {
      if (!inherits(args[[i]], "units"))
        stop(paste("argument", i, "is not of class units"))
      if (!ud.are.convertible(units(args[[i]]), u))
        stop(paste("argument", i, 
                   "has units that are not convertible to that of the first argument"))
      args[[i]] = as.units(args[[i]], u) # convert to first unit
    }
  }
  args = lapply(args, unclass)
  # as.units(do.call(.Generic, args), u)
  as.units(do.call(.Generic, c(args, na.rm = na.rm)), u)
}

#' @export
print.units <- function(x, digits = getOption("digits"), ...) # nocov start
{
  if (is.array(x) || length(x) > 1L) {
    cat("Units: ", as.character(attr(x, "units")), "\n", sep = "")
    y <- unclass(x)
    attr(y, "units") <- NULL
    print(y)
  } else { 
    u = as.character(attr(x, "units"))
    if (u == "")
      u = "(Units: 1)"
    cat(format(unclass(x), digits = digits), " ", u, "\n", sep = "")
  }
  invisible(x)
} # nocov end


#' @export
weighted.mean.units <- function(x, w, ...) 
  structure(weighted.mean(unclass(x), w, ...), 
            units = attr(x, "units"), class = "units")


#' @export
mean.units = function(x, ...) {
  .as.units(mean(unclass(x), ...), units(x))
}

#' @export
median.units = function(x, na.rm = FALSE, ...) {
}

median.units <- if (is.na(match("...", names(formals(median))))) {
    function(x, na.rm = FALSE) {
  		.as.units(median(unclass(x), na.rm = na.rm), units(x))
    }
} else {
    function(x, na.rm = FALSE, ...) {
  		.as.units(median(unclass(x), na.rm = na.rm, ...), units(x))
    }
}


#' @export
quantile.units = function(x, ...) {
  .as.units(quantile(unclass(x), ...), units(x))
}

#' @export
format.units = function(x, ...) {
  paste(format(unclass(x), ...), units(x))
}

#' @export
summary.units = function(object, ...) { 
  summary(unclass(object), ...)
}
