#include <RcppCommon.h>
using namespace Rcpp;

class SymbolicUnits {
public:
  SymbolicUnits(std::string_view x, bool strict=false)
    : x(x), it(x.begin()), strict(strict) { tokenize(); }
  SymbolicUnits(std::string_view x, std::string_view::iterator start, std::string_view::iterator end)
    : x(x.substr(start - x.begin(), end - start)), it(this->x.end()) {
    numerator.push_back(std::string(this->x));
  }
  operator SEXP();

private:
  std::string_view x;
  std::string_view::iterator it;
  std::vector<std::string> numerator, denominator;
  bool in_denominator = false;
  bool stay = false;
  bool strict = false;

  /* helpers -----------------------------------------------------------------*/

  bool is_multiplicative(const char& c) {
    return c == '*' || c == '.' || c == ' ';
  }

  bool is_number_char(const char& c) {
    return std::isdigit(c) || c == '.' || c == 'e' || c == '-';
  }

  bool is_symbol_char(const char& c) {
    if (!std::isdigit(c)) return !is_multiplicative(c) && c != '/' &&
      c != '(' && c != ')' && c != '^' && c != '-' && c != '+';

    auto lookahead = it;
    for (; lookahead != x.end() && std::isdigit(*lookahead); ++lookahead);
    if (lookahead != x.end() && is_symbol_char(*lookahead))
      return true;
    return false;
  }

  /* getters -----------------------------------------------------------------*/

  SymbolicUnits get_paren() {
    int level = 1;
    auto start = ++it;

    for (; it != x.end(); ++it) {
      const char& c = *it;
      if (c == '(')
        ++level;
      else if (c == ')' && --level == 0)
        break;
    }

    if (level != 0)
      stop("unmatched parenthesis");

    return SymbolicUnits(x.substr(start - x.begin(), it++ - start), strict);
  }

  int get_exponent() {
    int exponent = 1;
    if (it == x.end()) return exponent;

    bool is_negative = false;
    bool is_exponent = false;
    if (*it == '^') {
      is_exponent = true;
      ++it;
    }
    switch (*it) {
    case '-':
      is_negative = true;
    case '+':
      is_exponent = true;
      ++it;
    }
    bool is_digit = it != x.end() && std::isdigit(*it);

    if (!is_digit && is_exponent)
      stop("invalid exponent");
    else if (is_digit) {
      exponent = *(it++) - '0';
      for (; it != x.end() && isdigit(*it); ++it)
        exponent = exponent * 10 + (*it - '0');
      if (is_negative) exponent = -exponent;
    }

    return exponent;
  }

  SymbolicUnits get_number() {
    auto start = it++;
    for (; it != x.end() && is_number_char(*it); ++it);
    stay = !strict;
    return SymbolicUnits(x, start, it);
  }

  SymbolicUnits get_symbol() {
    auto start = (*it == '`') ? ++it : it++;
    for (; it != x.end() && is_symbol_char(*it); ++it);
    return SymbolicUnits(x, start, it - (*(it-1) == '`' ? 1 : 0));
  }

  /* main logic --------------------------------------------------------------*/

  void combine(SymbolicUnits& o, int exponent = 1) {
    bool invert = in_denominator != (exponent < 0);
    auto& target_num = invert ? denominator : numerator;
    auto& target_den = invert ? numerator : denominator;

    for (exponent = std::abs(exponent); exponent > 0; --exponent) {
      target_num.insert(target_num.end(), o.numerator.begin(), o.numerator.end());
      target_den.insert(target_den.end(), o.denominator.begin(), o.denominator.end());
    }
  }

  void tokenize() {
    while (it != x.end()) {
      const char& c = *it;

      // handle separators
      if (is_multiplicative(c)) {
        ++it;
        continue;
      }
      if (c == '/') {
        in_denominator = true;
        ++it;
        continue;
      }

      // handle parenthesis, digits, and symbols/names
      auto token = (c == '(') ? get_paren() :
        (std::isdigit(c) ? get_number() : get_symbol());
      combine(token, get_exponent());

      // switch to numerator if needed
      if (!stay) in_denominator = false;
      stay = false;
    }
  }
};

#include <Rcpp.h>

SymbolicUnits::operator SEXP() {
  List unit = List::create(
    _["numerator"]   = numerator,
    _["denominator"] = denominator
  );
  unit.attr("class") = "symbolic_units";
  return unit;
};

// [[Rcpp::export]]
SEXP parse_unit(std::string_view x, bool strict=false) {
  return SymbolicUnits(x, strict);
}

/*** R
parse_unit("m")
parse_unit("m2")
parse_unit("m2 / g^3")
parse_unit("m2 / g-1")
parse_unit("ml / min / 1.73 / m^2")
parse_unit("ml/min/1.73m^2")
parse_unit("ml/min/1.73/m^2")
parse_unit("ml/min/(1.73m^2)")
parse_unit("inH2O")
parse_unit("inH2O2")
*/
