
# Using environments as hash tables...
conversion_table <- new.env(parent = emptyenv())

get_conversion_function <- function(from, to) {
  if (!exists(from, conversion_table)) return(NULL)
  table <- get(from, conversion_table)
  if (!exists(to, table)) return(NULL)
  get(to, table)
}

user_are_convertible <- function(from, to) {
  f <- get_conversion_function(from, to)
  !is.null(f) && is.function(f)
}

user_convert <- function(value, from, to) {
  f <- get_conversion_function(from, to)
  f(value)
}


install_conversion_function <- function(from, to, f) {
  if (!exists(from, conversion_table)) {
    assign(from, new.env(parent = emptyenv()), conversion_table)
  }
  table <- get(from, conversion_table)
  assign(to, f, table)
}

install_conversion_constant <- function(from, to, const, offset = 0) {
  install_conversion_function(from, to, function(x) const * x + offset)
  install_conversion_function(to, from, function(x) (x - offset) / const)
}

