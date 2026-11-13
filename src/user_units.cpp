/*
  Installation and removal of user-defined units

  Part of the R interface to the udunits (API version 2) library
*/

#include "udunits.h"
#include <cctype>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

using namespace Rcpp;
using units::sys;
using units::enc;

/* User-defined units *********************************************************/

// udunits keeps two kinds of mappings:
//  - id -> unit: one map for symbols (case-sensitive) and one for names
//    (case-insensitive); the encoding argument is ignored.
//  - unit -> id: one tree per encoding (ASCII, Latin-1, UTF-8). Lookups in
//    Latin-1 fall back to ASCII, and lookups in UTF-8 fall back to Latin-1 and
//    then ASCII, so an ASCII id stored in the ASCII tree is found in every
//    encoding.
//
// install_unit() applies every mapping or none, and records the mappings it
// made, so that remove_unit() undoes exactly those and never touches mappings
// from the database or from another install.

namespace {

enum Kind { SYMBOL = 0, NAME = 1 };

struct Mapping {
  Kind kind;
  bool to_id;       // true: unit -> id; false: id -> unit
  std::string id;
  ut_encoding tree; // unit -> id only: the tree that holds it
};

typedef std::shared_ptr<ut_unit> unit_ptr;

struct Entry {
  unit_ptr unit;
  std::string id;   // as installed (names are keyed in lower case)
  long seq;         // install order, to promote the oldest alias
  std::vector<Mapping> maps;
};

std::map<std::string, Entry> registry[2];
long registry_seq = 0;

// Silences udunits' error handler while in scope: failures are reported
// through return codes instead of an R error thrown from inside udunits.
struct QuietUdunits {
  ut_error_message_handler prev;
  QuietUdunits() : prev(ut_set_error_message_handler(ut_ignore)) {}
  ~QuietUdunits() { ut_set_error_message_handler(prev); }
};

const char *kind_str(Kind kind) {
  return kind == SYMBOL ? "symbol" : "name";
}

std::string trim(const char *s) {
  std::string str(s);
  const char *ws = " \t\n\v\f\r";
  size_t first = str.find_first_not_of(ws);
  if (first == std::string::npos)
    return "";
  return str.substr(first, str.find_last_not_of(ws) - first + 1);
}

std::string key(Kind kind, const std::string &id) {
  if (kind == SYMBOL)
    return id;
  std::string out(id);
  for (size_t i = 0; i < out.size(); i++)
    if (!(out[i] & 0x80))
      out[i] = std::tolower(out[i]);
  return out;
}

bool is_ascii(const std::string &id) {
  for (size_t i = 0; i < id.size(); i++)
    if (id[i] & 0x80)
      return false;
  return true;
}

// the tree in which udunits stores a unit -> id mapping for this id
ut_encoding tree_for(const std::string &id) {
  if (is_ascii(id))
    return UT_ASCII;
  return enc == UT_UTF8 ? UT_UTF8 : UT_LATIN1;
}

// trimmed, non-empty and unique ids, in order
std::vector<std::string> clean(Kind kind, CharacterVector ids) {
  std::vector<std::string> out;
  std::set<std::string> seen;
  for (int i = 0; i < ids.size(); i++) {
    if (CharacterVector::is_na(ids[i]))
      stop("a %s cannot be NA", kind_str(kind));
    std::string id = trim(CHAR(STRING_ELT(ids, i)));
    if (id.empty())
      stop("a %s cannot be empty", kind_str(kind));
    if (seen.insert(key(kind, id)).second)
      out.push_back(id);
  }
  return out;
}

unit_ptr lookup(Kind kind, const std::string &id) {
  ut_unit *u = kind == SYMBOL ?
    ut_get_unit_by_symbol(sys, id.c_str()) : ut_get_unit_by_name(sys, id.c_str());
  return unit_ptr(u, ut_free);
}

const char *get_id(Kind kind, const ut_unit *unit, ut_encoding e) {
  return kind == SYMBOL ? ut_get_symbol(unit, e) : ut_get_name(unit, e);
}

// Whether the unit maps to an id in any tree. Querying ASCII and Latin-1
// first means the UTF-8 query only runs when there is nothing to fall back
// to, so it does not cache a converted copy of a Latin-1 id.
bool has_id(Kind kind, const ut_unit *unit) {
  return get_id(kind, unit, UT_ASCII) || get_id(kind, unit, UT_LATIN1) ||
    get_id(kind, unit, UT_UTF8);
}

ut_status apply(const Mapping &m, const ut_unit *unit) {
  const char *id = m.id.c_str();
  if (m.to_id)
    return m.kind == SYMBOL ?
      ut_map_unit_to_symbol(unit, id, m.tree) : ut_map_unit_to_name(unit, id, m.tree);
  return m.kind == SYMBOL ?
    ut_map_symbol_to_unit(id, enc, unit) : ut_map_name_to_unit(id, enc, unit);
}

void undo(const Mapping &m, const ut_unit *unit) {
  if (m.to_id) {
    // only if it still holds this id: the tree is looked up directly for
    // ASCII, and with fallbacks otherwise, so compare before removing
    const char *cur = get_id(m.kind, unit, m.tree);
    if (cur == NULL || m.id != cur)
      return;
    if (m.kind == SYMBOL)
      ut_unmap_unit_to_symbol(unit, m.tree);
    else ut_unmap_unit_to_name(unit, m.tree);
  } else {
    if (m.kind == SYMBOL)
      ut_unmap_symbol_to_unit(sys, m.id.c_str(), enc);
    else ut_unmap_name_to_unit(sys, m.id.c_str(), enc);
  }
}

// Plan the mappings needed to install ids of one kind for a unit. An id that
// already maps to the same unit needs nothing; one that maps to a different
// unit is refused here, before anything is mapped. The unit gets a symbol or
// name of its own only if it has none yet; otherwise the ids are aliases.
void plan(Kind kind, const std::vector<std::string> &ids, const ut_unit *unit,
          std::vector<Mapping> &out) {
  for (size_t i = 0; i < ids.size(); i++) {
    unit_ptr other = lookup(kind, ids[i]);
    if (other) {
      if (ut_compare(other.get(), unit) != 0)
        stop("%s \"%s\" already maps to existing but different unit",
             kind_str(kind), ids[i].c_str());
      continue;
    }
    Mapping m = {kind, false, ids[i], UT_ASCII};
    out.push_back(m);
  }
  if (!ids.empty() && !has_id(kind, unit)) {
    Mapping m = {kind, true, ids[0], tree_for(ids[0])};
    out.push_back(m);
  }
}

void record(const std::vector<Mapping> &done, const ut_unit *unit) {
  unit_ptr u(ut_clone(unit), ut_free);
  long seq = ++registry_seq;
  for (size_t i = 0; i < done.size(); i++) {
    const Mapping &m = done[i];
    Entry &e = registry[m.kind][key(m.kind, m.id)];
    if (!e.unit) {
      e.unit = u;
      e.id = m.id;
      e.seq = seq;
    }
    e.maps.push_back(m);
  }
}

// After removing the symbol or name of a unit, give it the one of its oldest
// remaining installed alias, if any.
void promote(Kind kind, const ut_unit *unit) {
  if (has_id(kind, unit))
    return;
  Entry *best = NULL;
  std::map<std::string, Entry>::iterator it;
  for (it = registry[kind].begin(); it != registry[kind].end(); ++it)
    if (ut_compare(it->second.unit.get(), unit) == 0 &&
        (best == NULL || it->second.seq < best->seq))
      best = &it->second;
  if (best == NULL)
    return;
  Mapping m = {kind, true, best->id, tree_for(best->id)};
  if (apply(m, best->unit.get()) == UT_SUCCESS)
    best->maps.push_back(m);
}

} // namespace

void units::registry_clear() {
  registry[SYMBOL].clear();
  registry[NAME].clear();
}

// [[Rcpp::export(rng=false)]]
void ud_install(CharacterVector symbols, CharacterVector names, xut_unit unit) {
  if (!unit)
    stop("invalid unit");
  std::vector<std::string> syms = clean(SYMBOL, symbols);
  std::vector<std::string> nms = clean(NAME, names);

  QuietUdunits quiet;
  std::vector<Mapping> todo, done;
  plan(SYMBOL, syms, unit, todo);
  plan(NAME, nms, unit, todo);

  for (size_t i = 0; i < todo.size(); i++) {
    if (apply(todo[i], unit) != UT_SUCCESS) {
      for (size_t j = done.size(); j-- > 0; )
        undo(done[j], unit);
      stop("could not map %s \"%s\"", kind_str(todo[i].kind), todo[i].id.c_str());
    }
    done.push_back(todo[i]);
  }
  record(done, unit);
}

// Undo the mappings made when installing these ids. Returns the ids that map
// to a unit but were not installed by ud_install(), which are left untouched.
// [[Rcpp::export(rng=false)]]
CharacterVector ud_remove(CharacterVector symbols, CharacterVector names) {
  std::vector<std::string> ids[2] = {clean(SYMBOL, symbols), clean(NAME, names)};
  CharacterVector foreign;

  QuietUdunits quiet;
  for (int k = SYMBOL; k <= NAME; k++) {
    Kind kind = (Kind) k;
    for (size_t i = 0; i < ids[k].size(); i++) {
      std::map<std::string, Entry>::iterator it = registry[k].find(key(kind, ids[k][i]));
      if (it == registry[k].end()) {
        if (lookup(kind, ids[k][i]))
          foreign.push_back(ids[k][i]);
        continue;
      }
      Entry e = it->second;
      registry[k].erase(it);
      bool primary = false;
      for (size_t j = e.maps.size(); j-- > 0; ) {
        undo(e.maps[j], e.unit.get());
        primary = primary || e.maps[j].to_id;
      }
      if (primary)
        promote(kind, e.unit.get());
    }
  }
  return foreign;
}
