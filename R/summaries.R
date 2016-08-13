#' @export
Summary.units = function(..., na.rm = FALSE) {
  OK <- switch(.Generic, "sum" = , "min" = , "max" = , "range" = TRUE, FALSE)
  if (!OK)
    stop(paste("Summary operation", .Generic, "not allowed"))
  # NextMethod(.Generic)
  args = list(...)
  u = units(args[[1]])
  if (length(args) > 1)
    for (i in 2:length(args)) {
      if (!inherits(args[[i]], "units"))
        stop(paste("argument", i, "is not of class units"))
      if (!ud.are.convertible(units(args[[i]]), u))
        stop(paste("argument", i, 
                   "has units that are not convertible to that of the first argument"))
      args[[i]] = as.units(args[[i]], u) # convert to first unit
    }
  args = lapply(args, unclass)
  as.units(do.call(.Generic, args), u)
}

#' @export
print.units <- function(x, digits = getOption("digits"), ...) 
{
  if (is.array(x) || length(x) > 1L) {
    cat("Units: ", attr(x, "units"), "\n", sep = "")
    y <- unclass(x)
    attr(y, "units") <- NULL
    print(y)
  } else { 
    u = attr(x, "units")
    if (u == "1")
      u = "(Units: 1)"
    cat(format(unclass(x), digits = digits), " ", u, "\n", sep = "")
  }
  invisible(x)
}


#' @export
weighted.mean.units <- function(x, w, ...) 
  structure(weighted.mean(unclass(x), w, ...), units = attr(x, 
                                                            "units"), class = "units")

#' @export
c.units <- function(..., recursive = FALSE) {
  args <- list(...)
  u = units(args[[1]])
  if (length(args) > 1)
    for (i in 2:length(args)) {
      if (!inherits(args[[i]], "units"))
        stop(paste("argument", i, "is not of class units"))
      if (!ud.are.convertible(units(args[[i]]), u))
        stop(paste("argument", i, 
                   "has units that are not convertible to that of the first argument"))
      units(args[[i]]) = u
    }
  x = unlist(args)
  as.units(x, u)
}

#' @export
as.data.frame.units = as.data.frame.numeric

#' convert units object into difftime object
#'
#' @param x object of class \code{units}
#'
#' @export
#' 
#' @examples
#' 
#' t1 = Sys.time() 
#' t2 = t1 + 3600 
#' d = t2 - t1
#' as.units(d)
#' (du = as.units(d, "d"))
#' dt = as.dt(du)
#' class(dt)
#' dt
as.dt = function(x) {
  stopifnot(inherits(x, "units"))
  u = units(x)
  if (u == "s")
    as.difftime(x, units = "secs")
  else if (u == "m")
    as.difftime(x, units = "mins")
  else if (u == "h")
    as.difftime(x, units = "hours")
  else if (u == "d")
    as.difftime(x, units = "days")
  else
    stop(paste("cannot convert unit", u, "to difftime object"))
}

#' @export
mean.units = function(x, ...) {
  .as.units(mean(unclass(x), ...), units(x))
}

#' @export
median.units = function(x, na.rm = FALSE) {
  .as.units(median(unclass(x), na.rm = na.rm), units(x))
}

#' @export
quantile.units = function(x, ...) {
  .as.units(quantile(unclass(x), ...), units(x))
}

#' @export
format.units = function(x, ...) {
  paste(format(unclass(x), ...), units(x))
}
