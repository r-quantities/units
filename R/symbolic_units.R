
.symbolic_units <- function(numerator, denominator = vector("character")) {
  structure(list(numerator = numerator,
                 denominator = denominator),
            class = "symbolic_units")
}

.multiply_symbolic_units <- function(value, e1, e2) {
  numerator <- sort(c(e1$numerator, e2$numerator))
  denominator <- sort(c(e1$denominator, e2$denominator))
  .simplify_units(value, .symbolic_units(numerator, denominator))
}

.invert_symbolic_units <- function(e) {
  .symbolic_units(e$denominator, e$numerator)
}

.divide_symbolic_units <- function(value, e1, e2) {
  .multiply_symbolic_units(value, e1, .invert_symbolic_units(e2))
}

.same_units <- function(e1, e2) {
  identical(e1$numerator, e2$numerator) && identical(e1$denominator, e2$denominator)
}

# Inside the group generic functions we do have .Generic even if the diagnostics
# think we do not.
# !diagnostics suppress=.Generic
#' @export
Ops.symbolic_units <- function(e1, e2) {
  if (nargs() == 1)
    stop(paste("unary", .Generic, "not defined for \"units\" objects"))

  eq <- switch(.Generic, "==" = , "!=" = TRUE, FALSE)
  if (eq) {
    if (.Generic == "==")
      .same_units(e1, e2)
    else
      !.same_units(e1, e2)
  } else
    stop(paste("operation", .Generic, "not allowed for symbolic operators")) # nocov
}

#' The "unit" type for vectors that are actually dimension-less.
#' @export
unitless <- .symbolic_units(vector("character"), vector("character"))

.pretty_print_sequence <- function(terms, op, neg_power = FALSE, sep = "") {
  pwr_op <- if (op == " ") "" else "^"
  sym <- unique(terms)
  pwr <- tabulate(factor(terms, sym))
  if (neg_power) pwr <- pwr * -1

  for (i in seq_along(sym)) if (pwr[i] != 1)
    sym[i] <- paste(sym[i], pwr[i], sep = pwr_op)
  paste0(sym, collapse = paste0(op, sep))
}

#' @export
as.character.symbolic_units <- function(x, ...,
		neg_power = get(".units.negative_power", envir = .units_options),
		escape_units = FALSE, prod_sep = "*", plot_sep = "") {
  sep <- plot_sep

  numerator <- x$numerator
  denominator <- x$denominator
  if (escape_units) {
    numerator <- unlist(Map(function(name) paste0("`", name, "`", sep = ""), numerator))
    denominator <- unlist(Map(function(name) paste0("`", name, "`", sep = ""), denominator))
  }

  if (x == unitless) { # xxx
	 u <- if (escape_units)
       unlist(Map(function(name) paste0("`", name, "`", sep = ""),
	      units_options("unitless_symbol")))
	 else
	    units_options("unitless_symbol")
	 return(u)
  }

  num_str <- if (length(numerator) > 0)
      .pretty_print_sequence(numerator, prod_sep, FALSE, plot_sep)
    else  { # only denominator:
      if (! neg_power)
	    "1" # 1/cm^2/h
	  else
	    character(0)
    }

  denom_str <- if (length(denominator) > 0) {
    sep <- if (neg_power)
      paste0(prod_sep, plot_sep) else "/"
    .pretty_print_sequence(denominator, sep, neg_power, plot_sep)
  } else
    character(0)

  if (length(num_str) == 0)
    denom_str
  else if (length(denom_str) == 0)
    num_str
  else
    paste(num_str, denom_str, sep = sep)
}

.simplify_units <- function(value, sym_units) {
  # from R >= 3.5
  isFALSE <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && !x

  if (isFALSE(.units.simplify())) {
  	value = unclass(value)
	  units(value) = sym_units
  	return(value)
  }

  # This is just a brute force implementation that takes each element in the
  # numerator and tries to find a value in the denominator that can be converted
  # to the same unit. It modifies "value" to rescale the nominator to the denominator
  # before removing matching units.

  drop_ones = function(u) u[ u != "1" ]
  class(value) <- "units"

  new_numerator <- drop_ones(sym_units$numerator)
  new_denominator <- drop_ones(sym_units$denominator)
  delete_num <- c()
  for (i in seq_along(new_numerator)) {
    str1 <- new_numerator[i]

    for (j in seq_along(new_denominator)) {
      str2 <- new_denominator[j]

      if (ud_are_same(str1, str2)) {
        attr(value, "units") <- units(as_units(str1))
        units(value) <- str2
        delete_num <- c(delete_num, i)
        new_denominator <- new_denominator[-j]
        break
      }

    }
  }
  if (length(delete_num) > 0)
    new_numerator <- new_numerator[-delete_num]

  as_units(drop_units(value), .symbolic_units(new_numerator, new_denominator))
}

#' Convert units to their base units
#'
#' Convert the units of a \code{units} object to their base units, as defined by
#' the udunits database (SI units).
#'
#' @param x object of class \code{units}.
#' @param simplify logical; if TRUE (default), the resulting units are simplified.
#' @param keep_fraction logical; if TRUE (default), the result is kept as a fraction.
#'
#' @return object of class \code{units} with units converted to base units.
#' @export
#'
#' @examples
#' x <- set_units(32, mJ/g)
#' convert_to_base(x)
#' convert_to_base(x, keep_fraction=FALSE)
#' convert_to_base(x, simplify=FALSE)
#' convert_to_base(x, simplify=FALSE, keep_fraction=FALSE)
convert_to_base <- function(x, simplify = TRUE, keep_fraction = TRUE) {
  stopifnot(inherits(x, "units"))

  u_strBase <- function(u_str, simplify) {
    u_new <- ud_parse(u_str, names=FALSE, definition=TRUE, ascii=TRUE)
    u_new <- strsplit(x = u_new, split = " @ ")[[1]][1]
    u_new <- strsplit(x = u_new, split = " ")[[1]]
    u_new <- u_new[length(u_new)]

    if (simplify)
      u_new <- ud_parse(u_new, names=FALSE, definition=FALSE, ascii=TRUE)
    gsub(".", " ", u_new, fixed = TRUE)
  }

  u <- vapply(units(x), paste0, character(1L), collapse = "*", recycle0=TRUE)
  u[u == ""] <- "1"

  u["numerator"]   <- sprintf("(%s)", u["numerator"])
  u["denominator"] <- sprintf("(%s)", u["denominator"])

  if (!keep_fraction) u <- paste(u, collapse = "/")

  u_base <- vapply(u, u_strBase, character(1L), simplify = simplify)

  if (keep_fraction) {
    is_unitless <- u_base == "1"

    u_base["numerator"]   <- sprintf("(%s)", u_base["numerator"])
    u_base["denominator"] <- sprintf("(%s)-1", u_base["denominator"])

    u_base <- paste(u_base[!is_unitless], collapse = " ")
  }

  set_units(x, u_base, mode = "standard")
}
