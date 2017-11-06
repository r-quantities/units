#' @export
c.units <- function(..., recursive = FALSE) {
  args <- list(...)
  u = units(args[[1]])
  if (length(args) > 1)
    for (i in 2:length(args)) {
      if (!inherits(args[[i]], "units"))
        stop(paste("argument", i, "is not of class units"))
      tr = try(units(args[[i]]) <- u)
      if (class(tr) == "try-error")
        stop(paste("argument", i, 
                   "has units that are not convertible to that of the first argument"))
    }
  x = unlist(args)
  as_units(x, u)
}

.as.units = function(x, value) {
  x = unclass(x)
  class(x) = "units"
  attr(x, "units") = value
  x
}

#' @export
diff.units = function(x, ...) { 
  u = units(x)
  # units(x) = u will not work here, as units(x) is NULL!
  .as.units(NextMethod(), u)
}

#' @export
rep.units = function(x, ...) {
  u = units(x)
  .as.units(NextMethod(), u)
}




.parse_unit_with_implicit_exponents <- function(str) {
  if (length(grep(c("[*/]"), str)) > 0)
    stop("If 'implicit_exponents = TRUE', strings cannot contain `*' or `/'")
  parse_one = function(str) {
    r <- regexpr("[-0-9]+", str)
    if (r == -1)
      return(make_unit(str))
    power = as.integer(substr(str, r, nchar(str)))
    u = if (power < 0) {
	  subs = substr(str, 1, r-1) # word before power
      1/make_unit(subs) # word before power
    } else {
      subs = substr(str, 1, r-1)
      make_unit(subs)
	}
    if (abs(power) > 1) {
      u0 = u
      for (i in 2:abs(power))
        u = u * u0
      } 
      return(u)
    }
  if (str == "1")
     return(make_unit("1"))
  first = TRUE
  while ((r <- regexpr("[ ]+", str)) != -1) {
    this = substr(str, 1, r-1) # first word
    u = if (first) {
      first = FALSE
      parse_one(this)
    } else
      u * parse_one(this)
    str = substr(str, r+1, nchar(str))
  }
  if (first) # single unit
    parse_one(str)
  else
    u * parse_one(str)
}

#' deparse unit to string in product power form (e.g. km m-2 s-1)
#' 
#' deparse unit to string in product power form (e.g. km m-2 s-1)
#' @param x object of class units
#' @return length one character vector
#' @examples 
#' u = parse_unit("kg m-2 s-1")
#' u
#' deparse_unit(u)
#' @export
deparse_unit = function(x) {
  stopifnot(inherits(x, "units"))
  u = units(x)
  tn = table(u$numerator)
  nm1 = names(tn)
  vals1 = as.character(tn)
  vals1[vals1 == "1"] = ""
  td = - table(u$denominator)
  nm2 = names(td)
  vals2 = as.character(td)
  paste(c(paste0(nm1, vals1), paste0(nm2, vals2)), collapse=" ")
}

#' @export
#' @name deparse_unit
#' @details \code{as_cf} is deprecated; use \code{deparse_unit}.
as_cf = function(x) {
  .Deprecated("deparse_unit") # nocov
  deparse_unit(x)             # nocov
}

#' @method all.equal units
#' @export
all.equal.units = function(target, current, ...) {
  current = set_units(current, units(target), mode = "units")
  all.equal(unclass(target), unclass(current), ...)
}

#' type_sum for tidy tibble printing
#' 
#' type_sum for tidy tibble printing
#' @param x object of class units
#' @param ... ignored
#' @export
type_sum.units <- function(x, ...) {
  "units"
}

#' seq method for units objects
#' @param from see \link[base]{seq}
#' @param to see \link[base]{seq}
#' @param by see \link[base]{seq}
#' @param length.out see \link[base]{seq}
#' @param along.with see \link[base]{seq}
#' @param ... see \link[base]{seq}
#' @details arguments with units are converted to have units of the first argument (which is either \code{from} or \code{to})
#' @export
#' @examples
#' seq(to = set_units(10, m), by = set_units(1, m), length.out = 5)
#' seq(set_units(10, m), by = set_units(1, m), length.out = 5)
#' seq(set_units(10, m), set_units(19, m))
#' seq(set_units(10, m), set_units(.1, km), set_units(10000, mm))
seq.units = function(from, to, by = ((to - from)/(length.out - 1)),
         length.out = NULL, along.with = NULL, ...) {
  mf = missing(from)
  mt = missing(to)
  uuu = if (mf)
      units(to)
  	else
      units(from)
  if (! mf)
    from = as.numeric(from)
  if (! mt)
  	to = as.numeric(set_units(to, uuu, mode = "units"))
  if (! missing(by))
    by = as.numeric(set_units(by, uuu, mode = "units"))
  set_units(NextMethod(), uuu, mode = "units")
}

##' @export
#as.data.frame.units = function(x, ...) {
#	unitsx = units(x)
#	x = unclass(x) # FIXME: names are not passed on for single vectors, e.g. as.data.frame(u[,1])
#	ret = as.data.frame(x)
#	structure(lapply(ret, function(y) set_units(y, unitsx)),
#		row.names = 1:nrow(ret), 
#		class = "data.frame")
#}
