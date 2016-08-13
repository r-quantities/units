
.symbolic_units <- function(nominator, denominator) {
  structure(list(nominator = nominator, 
                 denominator = denominator), 
            class = "symbolic_units")
}

.invert_symbolic_units <- function(e) {
  .symbolic_units(e$denominator, e$nominator)
}

.multiply_symbolic_units <- function(e1, e2) {
  nominator <- sort(c(e1$nominator, e2$nominator))
  denominator <- sort(c(e1$denominator, e2$denominator))
  # fixme: reduce
  .symbolic_units(nominator, denominator)
}

.same_units <- function(e1, e2) {
  all(e1$nominator == e2$nominator) && all(e1$denominator == e2$denominator)
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
    if (.Generic == "==") return(.same_units(e1, e2))
    else return(!.same_units(e1, e2))
  }
  
  prd <- switch(.Generic, "*" = , "/" = TRUE, FALSE)
  if (!prd) stop(paste("operation", .Generic, "not allowed for symbolic operators"))
  
  if (!inherits(e1, "symbolic_units") || !inherits(e2, "symbolic_units")) {
    stop(paste("Arithmetic operations on symbolic units only possible ",  # nocov
               "if both arguments are symbolic units", sep = "\n"))       # nocov
  }
  
  if (.Generic == "*") .multiply_symbolic_units(e1, e2)              # multiplication
  else .multiply_symbolic_units(e1, .invert_symbolic_units(e2)) # division
}

.make_symbolic_units <- function(name) {
  .symbolic_units(name, vector("character"))
}

#' The "unit" type for vectors that are actually dimension-less.
#' @export
unitless <- .symbolic_units(vector("character"), vector("character"))

#' @export
as.character.symbolic_units <- function(x, ...) {
  nom_str <- ""
  sep <- ""
  denom_str <- ""
  
  if (length(x$nominator) == 0) {
    nom_str <- "1"
  } else if (length(x$nominator) == 1) {
    nom_str <- x$nominator
  } else {
    nom_str <- paste0(x$nominator, collapse = "*")
  }
  
  if (length(x$denominator) > 0) {
    sep = "/"
    if (length(x$denominator) == 1) {
      denom_str = x$denominator
    } else {
      denom_str <- paste0(x$denominator, collapse = "/")
    }
  }

  paste0(nom_str, sep, denom_str)
}

#' Create a new unit from a unit name.
#' 
#' @param name  Name of the new unit
#' @return A new unit object that can be used in arithmetics
#' 
#' @export
make_unit <- function(name) {
  as.units.default(1, .make_symbolic_units(name))
}

.get_conversion_constant <- function(u1, u2) {
  # FIXME: Unit conversion only has limited support right now
  # I always just ask ud to convert units.
  su1 <- as.character(u1)
  su2 <- as.character(u2)
  
  if (!udunits2::ud.are.convertible(su1, su2)) return(NA)
  ud.convert(1, su1, su2)
}
