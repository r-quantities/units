
# Inside the group generic functions we do have .Generic even if the diagnostics
# think we do not.
# !diagnostics suppress=.Generic

#' Title
#'
#' @param e1 object of class \code{units}, 
#'        or something that can be coerced to it by \code{as.units(e1)}
#' @param e2 object of class \code{units}, 
#'        or something that can be coerced to it by \code{as.units(e2)}
#'
#' @return object of class \code{units}
#' @export
#'
#' @examples
#' a = as.units(1:3, "m/s")
#' b = as.units(1:3, "m/s")
#' a + b
Ops.units <- function(e1, e2) {
  if (nargs() == 1)
    stop(paste("unary", .Generic, "not defined for \"units\" objects"))
  
  eq <- switch(.Generic, "+" = , "-" = , "==" = , "!=" = , 
               "<" = , ">" = , "<=" = , ">=" = TRUE, FALSE)
  
  prd <- switch(.Generic, "*" = , "/" = TRUE, FALSE)
  
  pw <- switch(.Generic, "**" = , "^" = TRUE, FALSE)
  
  if (!eq && !prd && !pw)
    stop(paste("operation", .Generic, "not allowed"))
  
  if (eq)
    units(e2) = units(e1) 
  
  if (prd && inherits(e1, "units") && inherits(e2, "units")) {
    attr(e1, "units") = paste0("(", units(e1), ")", .Generic, "(", units(e2), ")")
    attr(e2, "units") = paste0("(", units(e1), ")", .Generic, "(", units(e2), ")")
  }
  
  if (pw) {
    if (inherits(e2, "units") || length(e2) > 1L)
      stop("power operation only allowed with length-one numeric power")
    attr(e1, "units") = paste0("(", units(e1), ")", .Generic, e2)
  }
  NextMethod(.Generic)
}

