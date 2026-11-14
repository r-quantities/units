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
// from the database or from another install. With override = TRUE, it may
// also set aside the database mappings that give an id another meaning; they
// are recorded too, and remove_unit() puts them back.

namespace {

enum Kind { SYMBOL = 0, NAME = 1 };

typedef std::shared_ptr<ut_unit> unit_ptr;

struct Mapping {
  Kind kind;
  bool to_id;           // true: unit -> id; false: id -> unit
  std::string id;
  ut_encoding tree;     // unit -> id only: the tree that holds it
  Kind owner;           // the installed id this mapping belongs to
  std::string owner_id;
  unit_ptr aside;       // set for a database mapping set aside by an
                        // override: the unit it maps to or from
};

Mapping mapping(Kind kind, bool to_id, const std::string &id,
                ut_encoding tree = UT_ASCII) {
  Mapping m;
  m.kind = kind;
  m.to_id = to_id;
  m.id = id;
  m.tree = tree;
  m.owner = kind;
  m.owner_id = id;
  return m;
}

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

// What udunits reads an id as: by name (case-insensitive), then by symbol,
// then the same after stripping a prefix. An id of one kind can thus shadow
// one of the other kind, or a prefixed unit.
unit_ptr parse(const std::string &id) {
  return unit_ptr(ut_parse(sys, id.c_str(), enc), ut_free);
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

// For a mapping set aside, apply() removes it and undo() puts it back.
ut_status apply(const Mapping &m, const ut_unit *unit) {
  const char *id = m.id.c_str();
  if (m.aside) {
    if (m.to_id)
      return m.kind == SYMBOL ?
        ut_unmap_unit_to_symbol(m.aside.get(), m.tree) :
        ut_unmap_unit_to_name(m.aside.get(), m.tree);
    return m.kind == SYMBOL ?
      ut_unmap_symbol_to_unit(sys, id, enc) : ut_unmap_name_to_unit(sys, id, enc);
  }
  if (m.to_id)
    return m.kind == SYMBOL ?
      ut_map_unit_to_symbol(unit, id, m.tree) : ut_map_unit_to_name(unit, id, m.tree);
  return m.kind == SYMBOL ?
    ut_map_symbol_to_unit(id, enc, unit) : ut_map_name_to_unit(id, enc, unit);
}

void undo(const Mapping &m, const ut_unit *unit) {
  if (m.aside) {
    const char *id = m.id.c_str();
    if (m.to_id) {
      if (m.kind == SYMBOL)
        ut_map_unit_to_symbol(m.aside.get(), id, m.tree);
      else ut_map_unit_to_name(m.aside.get(), id, m.tree);
    } else {
      if (m.kind == SYMBOL)
        ut_map_symbol_to_unit(id, enc, m.aside.get());
      else ut_map_name_to_unit(id, enc, m.aside.get());
    }
  } else if (m.to_id) {
    // only if it still holds this id: the tree is looked up directly for
    // ASCII, and with fallbacks otherwise, so compare before removing
    const char *cur = get_id(m.kind, unit, m.tree);
    if (cur == NULL || m.id != cur)
      return; // #nocov: a unit gets its own id only when it has none
    if (m.kind == SYMBOL)
      ut_unmap_unit_to_symbol(unit, m.tree);
    else ut_unmap_unit_to_name(unit, m.tree);
  } else {
    if (m.kind == SYMBOL)
      ut_unmap_symbol_to_unit(sys, m.id.c_str(), enc);
    else ut_unmap_name_to_unit(sys, m.id.c_str(), enc);
  }
}

// Names are found whatever their case, so a new name also shadows every
// symbol that differs from it only in case (e.g., a name "KG" would make
// "kg" and "mkg" read as the new unit). udunits cannot look up symbols
// regardless of case, so refuse a name if any of its case variants is read
// as a different unit. No database symbol, prefixed or not, is longer than 8
// characters, so longer names are only checked against installed symbols.
// With override, this runs before anything is set aside, and a variant that
// is read as what the id itself meant is that same name, not another id.
bool shadowed(const ut_unit *u, const ut_unit *unit, const ut_unit *was) {
  return ut_compare(u, unit) != 0 && !(was && ut_compare(u, was) == 0);
}

void check_case_variants(const std::string &id, const ut_unit *unit,
                         const ut_unit *was = NULL) {
  std::vector<size_t> letters;
  for (size_t i = 0; i < id.size(); i++)
    if (std::isalpha((unsigned char) id[i]) && !(id[i] & 0x80))
      letters.push_back(i);

  if (letters.size() <= 12) {
    for (unsigned long mask = 0; mask < (1UL << letters.size()); mask++) {
      std::string v(id);
      for (size_t j = 0; j < letters.size(); j++)
        v[letters[j]] = (mask >> j) & 1 ?
          std::toupper(v[letters[j]]) : std::tolower(v[letters[j]]);
      if (v == id)
        continue;
      unit_ptr u = parse(v);
      if (u && shadowed(u.get(), unit, was))
        stop("name \"%s\" would shadow \"%s\", which maps to a different unit",
             id.c_str(), v.c_str());
    }
    return;
  }

  std::map<std::string, Entry>::iterator it;
  for (it = registry[SYMBOL].begin(); it != registry[SYMBOL].end(); ++it)
    if (key(NAME, it->first) == key(NAME, id) &&
        shadowed(it->second.unit.get(), unit, was))
      stop("name \"%s\" would shadow \"%s\", which maps to a different unit",
           id.c_str(), it->first.c_str());
}

std::string describe(const ut_unit *unit) {
  char buf[256];
  int len = ut_format(unit, buf, sizeof(buf), enc | UT_NAMES);
  if (len < 0)
    return "another unit"; // #nocov
  if (len < (int) sizeof(buf))
    return buf;
  // # nocov start
  std::vector<char> longer(len + 1);
  ut_format(unit, longer.data(), longer.size(), enc | UT_NAMES);
  return longer.data();
} // # nocov end

void put_aside(Mapping m, Kind owner, const std::string &id, unit_ptr old,
               const ut_unit *unit, std::vector<Mapping> &done) {
  m.owner = owner;
  m.owner_id = id;
  m.aside = old;
  apply(m, unit);
  done.push_back(m);
}

// With override, set aside, right away, the database mappings that give an
// id another meaning: the id in either map (a leftover name would win over a
// new symbol, and a leftover symbol could still be its unit's own), and the
// symbol or name of those units when it is this id (otherwise they would be
// printed as an id that is read as the new unit). A prefixed meaning, as in
// "km", needs nothing: the id itself wins over it.
void set_aside(Kind owner, const std::string &id, const ut_unit *unit,
               std::vector<Mapping> &done, std::vector<std::string> &was) {
  // what the id meant, by whatever route, before anything is set aside
  unit_ptr before = parse(id);
  if (before && ut_compare(before.get(), unit) != 0)
    was.push_back(std::string(kind_str(owner)) + " \"" + id + "\" (was " +
                  describe(before.get()) + ")");

  for (int k = SYMBOL; k <= NAME; k++) {
    Kind kind = (Kind) k;
    unit_ptr old = lookup(kind, id);
    if (!old || ut_compare(old.get(), unit) == 0)
      continue;
    if (registry[k].count(key(kind, id)))
      stop("%s \"%s\" was installed by install_unit(); remove it first",
           kind_str(kind), id.c_str());

    put_aside(mapping(kind, false, id), owner, id, old, unit, done);
    // one tree at a time: lookups in other trees fall back to ASCII, so the
    // ASCII entry has to be out of the way to see what the next tree holds
    const ut_encoding trees[2] = {UT_ASCII, enc};
    for (int t = 0; t < 2; t++) {
      const char *cur = get_id(kind, old.get(), trees[t]);
      if (cur && key(kind, cur) == key(kind, id))
        put_aside(mapping(kind, true, cur, trees[t]), owner, id, old, unit, done);
    }
  }
}

// Plan the mappings needed to install ids of one kind for a unit. An id that
// already maps to the same unit needs nothing; one that maps, or is read as,
// a different unit is refused here, before anything is mapped (with
// override, what remains after set_aside() is a prefixed meaning, which the
// new id takes over). The unit gets a symbol or name of its own only if it
// has none yet; otherwise the ids are aliases.
void plan(Kind kind, const std::vector<std::string> &ids, const ut_unit *unit,
          bool override, std::vector<Mapping> &out) {
  for (size_t i = 0; i < ids.size(); i++) {
    unit_ptr mapped = lookup(kind, ids[i]);
    unit_ptr other = mapped ? mapped : parse(ids[i]);
    if (other && ut_compare(other.get(), unit) != 0 && !override)
      stop("%s \"%s\" already maps to existing but different unit",
           kind_str(kind), ids[i].c_str());
    if (mapped)
      continue;
    if (kind == NAME && !override) // with override, checked before set_aside()
      check_case_variants(ids[i], unit);
    out.push_back(mapping(kind, false, ids[i]));
  }
  if (!ids.empty() && !has_id(kind, unit))
    out.push_back(mapping(kind, true, ids[0], tree_for(ids[0])));
}

void check_read_back(Kind kind, const std::vector<std::string> &ids,
                     const ut_unit *unit) {
  for (size_t i = 0; i < ids.size(); i++) {
    unit_ptr u = parse(ids[i]);
    if (!u || ut_compare(u.get(), unit) != 0)
      stop("%s \"%s\" cannot be installed: udunits does not read it as this unit",
           kind_str(kind), ids[i].c_str());
  }
}

void record(const std::vector<Mapping> &done, const ut_unit *unit) {
  unit_ptr u(ut_clone(unit), ut_free);
  long seq = ++registry_seq;
  for (size_t i = 0; i < done.size(); i++) {
    const Mapping &m = done[i];
    Entry &e = registry[m.owner][key(m.owner, m.owner_id)];
    if (!e.unit) {
      e.unit = u;
      e.id = m.owner_id;
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
  Mapping m = mapping(kind, true, best->id, tree_for(best->id));
  if (apply(m, best->unit.get()) == UT_SUCCESS)
    best->maps.push_back(m);
}

} // namespace

void units::registry_clear() {
  registry[SYMBOL].clear();
  registry[NAME].clear();
}

// Returns a description of each database mapping set aside by an override.
// [[Rcpp::export(rng=false)]]
CharacterVector ud_install(CharacterVector symbols, CharacterVector names,
                           xut_unit unit, bool override = false) {
  if (!unit)
    stop("invalid unit"); // #nocov: install_unit() always passes a valid unit
  std::vector<std::string> syms = clean(SYMBOL, symbols);
  std::vector<std::string> nms = clean(NAME, names);

  QuietUdunits quiet;
  std::vector<Mapping> done;
  std::vector<std::string> was;
  try {
    if (override) {
      for (size_t i = 0; i < nms.size(); i++) {
        unit_ptr was = parse(nms[i]);
        check_case_variants(nms[i], unit, was.get());
      }
      for (size_t i = 0; i < syms.size(); i++)
        set_aside(SYMBOL, syms[i], unit, done, was);
      for (size_t i = 0; i < nms.size(); i++)
        set_aside(NAME, nms[i], unit, done, was);
    }

    std::vector<Mapping> todo;
    plan(SYMBOL, syms, unit, override, todo);
    plan(NAME, nms, unit, override, todo);
    for (size_t i = 0; i < todo.size(); i++) {
      // conflicts are refused when planning, and ids in an invalid encoding
      // in install_unit(), so only a system error (UT_OS) can get here
      if (apply(todo[i], unit) != UT_SUCCESS)
        stop("could not map %s \"%s\"", kind_str(todo[i].kind), todo[i].id.c_str()); // #nocov
      done.push_back(todo[i]);
    }

    // every id must now be read as this unit; e.g., "a1" is read as "a" to
    // the power of 1
    check_read_back(SYMBOL, syms, unit);
    check_read_back(NAME, nms, unit);
  } catch (...) {
    for (size_t j = done.size(); j-- > 0; )
      undo(done[j], unit);
    throw;
  }
  record(done, unit);
  return wrap(was);
}

// Undo the mappings made when installing these ids. Returns the ids that
// udunits reads as a unit (e.g., "km", by prefix) but were not installed by
// ud_install(), which are left untouched.
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
        if (parse(ids[k][i]))
          foreign.push_back(ids[k][i]);
        continue;
      }
      Entry e = it->second;
      registry[k].erase(it);
      bool primary = false;
      // in reverse: this install's mappings first, then what it set aside
      for (size_t j = e.maps.size(); j-- > 0; ) {
        undo(e.maps[j], e.unit.get());
        primary = primary || (e.maps[j].to_id && !e.maps[j].aside);
      }
      if (primary)
        promote(kind, e.unit.get());
    }
  }
  return foreign;
}
