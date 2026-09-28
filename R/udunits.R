#' \pkg{udunits2} utilities
#'
#' Some \pkg{udunits2} utilities are exposed to the user. These functions are
#' useful for checking whether units are convertible or converting between units
#' without having to create \pkg{units} objects.
#' Arguments are recycled if necessary.
#'
#' @param from,to character vector or object of class \code{symbolic_units},
#' for the symbol(s) of the original unit(s) and the unit to convert to respectively.
#' @param ...  unused.
#'
#' @return \code{ud_are_convertible}
#' returns \code{TRUE} if both units exist and are convertible,
#' \code{FALSE} otherwise.
#'
#' @name udunits2
#' @export
ud_are_convertible <- function(from, to, ...) {
  if (length(dots <- list(...))) {
    if (exists("x", dots)) from <- dots$x
    if (exists("y", dots)) to   <- dots$y
    warning("variables `x` and `y` were unfortunate names, and are deprecated",
            "; please use `from` and `to` instead")
  }
  from <- ud_char(from)
  to <- ud_char(to)
  if (!ud_recycles(from, to))
    return(mapply(ud_convertible, from, to, USE.NAMES=FALSE))

  # one check per distinct pair of units
  n <- max(length(from), length(to))
  from <- rep_len(from, n)
  to <- rep_len(to, n)
  out <- logical(n)
  for (i in ud_pairs(from, to))
    out[i] <- ud_convertible(from[[i[1L]]], to[[i[1L]]])
  out
}

#' @param x numeric vector
#'
#' @return \code{ud_convert}
#' returns a numeric vector with \code{x} converted to new unit.
#'
#' @name udunits2
#' @export
#'
#' @examples
#' ud_are_convertible(c("m", "mm"), "km")
#' ud_convert(c(100, 100000), c("m", "mm"), "km")
#'
#' a <- set_units(1:3, m/s)
#' ud_are_convertible(units(a), "km/h")
#' ud_convert(1:3, units(a), "km/h")
#'
#' ud_are_convertible("degF", "degC")
#' ud_convert(32, "degF", "degC")
ud_convert <- function(x, from, to) {
  if (!length(x)) return(x)
  from <- ud_char(from)
  to <- ud_char(to)
  if (!is.atomic(x) || !ud_recycles(x, from, to))
    return(mapply(ud_convert_doubles, x, from, to))

  # one conversion per distinct pair of units, results in the original order
  n <- max(length(x), length(from), length(to))
  nm <- names(x)
  attributes(x) <- NULL
  if (length(x) != n) x <- rep_len(x, n)
  if (length(from) == 1L && length(to) == 1L) {
    out <- ud_convert_doubles(x, from, to)
  } else {
    from <- rep_len(from, n)
    to <- rep_len(to, n)
    out <- numeric(n)
    for (i in ud_pairs(from, to))
      out[i] <- ud_convert_doubles(x[i], from[[i[1L]]], to[[i[1L]]])
  }
  names(out) <- nm # as mapply() names its result
  out
}

# Whether the arguments recycle to a common length without mapply()'s
# zero-length and not-a-multiple cases, which are left to mapply() itself.
ud_recycles <- function(...) {
  len <- lengths(list(...))
  all(len > 0L) && all(max(len) %% len == 0L)
}

# Indices of each distinct (from, to) pair, in order of first appearance.
ud_pairs <- function(from, to) {
  uf <- unique(from)
  pair <- match(from, uf) + length(uf) * (match(to, unique(to)) - 1) # double: no overflow
  split(seq_along(pair), match(pair, unique(pair)))
}

ud_char <- function(x) {
  if (is.character(x)) return(x)
  if (!inherits(x, "symbolic_units")) stop("not a unit")

  # using * instead of space to workaround #414
  res <- if (length(x$numerator))
    paste(x$numerator, collapse="*") else "1"
  if (length(x$denominator))
    res <- paste0(res, "*(", paste(x$denominator, collapse="*"), ")-1")
  res
}

ud_are_same <- function(x, y) {
  get_base <- function(x)
    tail(strsplit(R_ut_format(R_ut_parse(x), definition=TRUE), " ")[[1]], 1)
  identical(get_base(x), get_base(y))
}

ud_get_symbol = function(u) {
  u <- R_ut_parse(u)
	sym = R_ut_get_symbol(u)
	if (!length(sym))
		sym = R_ut_get_name(u)
	sym
}

ud_is_parseable = function(u) {
	res <- try(R_ut_parse(u), silent = TRUE)
	! inherits(res, "try-error")
}

ud_parse <- function(u, names=FALSE, definition=FALSE, ascii=FALSE) {
  R_ut_format(R_ut_parse(u), names, definition, ascii)
}
