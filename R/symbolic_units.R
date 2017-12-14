
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
  all(e1$numerator == e2$numerator) && all(e1$denominator == e2$denominator)
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
  # `fix` handles cases where a unit is actually an expression. We would have to
  # deparse these to really do a pretty printing, but for now we leave them alone...
  fix <- function(term) {
    if (length(grep("/", term)) || length(grep("-", term)))
      paste0("(", term, ")")
    else
      term
  }
  fixed <- vapply(terms, fix, "")
  fixed_tbl <- table(fixed)
  
  names <- names(fixed_tbl)
  result <- vector("character", length(fixed_tbl))
  for (i in seq_along(fixed_tbl)) {
    name <- names[i]
    value <- fixed_tbl[i]
    if (value > 1 || (value == 1 && neg_power)) {
	  if (neg_power)
	  	value <- value * -1.
      result[i] <- paste0(name, "^", value)
    } else {
      result[i] <- name
    }
  }
  
  paste0(result, collapse = paste0(op, sep))
}

#' @export
as.character.symbolic_units <- function(x, ..., 
		neg_power = get(".units.negative_power", envir = .units_options), 
		escape_units = FALSE, plot_sep = "") {
  num_str <- character(0)
  denom_str <- character(0)
  sep <- plot_sep

  numerator <- x$numerator
  denominator <- x$denominator
  if (escape_units) {
    numerator <- unlist(Map(function(name) paste0("`", name, "`", sep = ""), numerator))
    denoinator <- unlist(Map(function(name) paste0("`", name, "`", sep = ""), denominator))
  }
  
  if (length(numerator) == 0) {
    if (! neg_power)
	  num_str <- "1" # 1/cm^2/h
  } else {
    num_str <- .pretty_print_sequence(numerator, "*", FALSE, plot_sep)
  }
  
  if (length(denominator) > 0) {
    sep <- if (neg_power)
	    paste0("*", plot_sep)
	  else
        "/"
    denom_str <- .pretty_print_sequence(denominator, sep, neg_power, plot_sep)
  }

  if (length(num_str) == 0) {
    if (length(denom_str) == 0)
	  ""
    else
	  denom_str
  } else {
    if (length(denom_str) == 0)
      num_str
    else
      paste(num_str, denom_str, sep = sep)
  }
}

.simplify_units <- function(value, sym_units) {
  
  # This is just a brute force implementation that takes each element in the
  # numerator and tries to find a value in the denominator that can be converted
  # to the same unit. It modifies "value" to rescale the nominator to the denomiator
  # before removing matching units.

  drop_ones = function(u) u[ u != "1" ]
  
  new_numerator <- drop_ones(sym_units$numerator)
  new_denominator <- drop_ones(sym_units$denominator)
  
  delete_num <- c()
  for (i in seq_along(new_numerator)) {
    str1 <- new_numerator[i]

    for (j in seq_along(new_denominator)) {
      str2 <- new_denominator[j]

      if (are_convertible(str1, str2)) {
        value <- convert(value, str1, str2)
        delete_num <- c(delete_num, i)
        new_denominator <- new_denominator[-j]
        break
      }
      
    }
  }
  if (length(delete_num) > 0)
    new_numerator <- new_numerator[-delete_num]
  
  as_units(value, .symbolic_units(new_numerator, new_denominator))
}
