#include "precompiled.hpp"

#include "ci/ciSymbol.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "memory/oopFactory.hpp"

// ------------------------------------------------------------------
// ciSymbol::ciSymbol
//
// Preallocated symbol variant.  Used with symbols from vmSymbols.
ciSymbol::ciSymbol(Symbol* s, vmSymbols::SID sid)
  : _symbol(s), _sid(sid)
{
  _symbol->increment_refcount();  // increment ref count
}

// Normal case for non-famous symbols.
ciSymbol::ciSymbol(Symbol* s)
  : _symbol(s), _sid(vmSymbols::NO_SID)
{
  _symbol->increment_refcount();  // increment ref count
}

// ciSymbol
//
// This class represents a Symbol* in the HotSpot virtual
// machine.

// ------------------------------------------------------------------
// ciSymbol::as_utf8
//
// The text of the symbol as a null-terminated C string.
const char* ciSymbol::as_utf8() {
  GUARDED_VM_QUICK_ENTRY(return get_symbol()->as_utf8();)
}

// The text of the symbol as a null-terminated C string.
const char* ciSymbol::as_quoted_ascii() {
  GUARDED_VM_QUICK_ENTRY(return get_symbol()->as_quoted_ascii();)
}

// ------------------------------------------------------------------
// ciSymbol::base
const jbyte* ciSymbol::base() {
  GUARDED_VM_ENTRY(return get_symbol()->base();)
}

// ------------------------------------------------------------------
// ciSymbol::byte_at
int ciSymbol::byte_at(int i) {
  GUARDED_VM_ENTRY(return get_symbol()->byte_at(i);)
}

// ------------------------------------------------------------------
// ciSymbol::starts_with
//
// Tests if the symbol starts with the given prefix.
bool ciSymbol::starts_with(const char* prefix, int len) const {
  GUARDED_VM_ENTRY(return get_symbol()->starts_with(prefix, len);)
}

bool ciSymbol::is_signature_polymorphic_name() const {
  GUARDED_VM_ENTRY(return MethodHandles::is_signature_polymorphic_name(get_symbol());)
}

// ------------------------------------------------------------------
// ciSymbol::index_of
//
// Determines where the symbol contains the given substring.
int ciSymbol::index_of_at(int i, const char* str, int len) const {
  GUARDED_VM_ENTRY(return get_symbol()->index_of_at(i, str, len);)
}

// ------------------------------------------------------------------
// ciSymbol::utf8_length
int ciSymbol::utf8_length() {
  GUARDED_VM_ENTRY(return get_symbol()->utf8_length();)
}

// ------------------------------------------------------------------
// ciSymbol::print_impl
//
// Implementation of the print method
void ciSymbol::print_impl(outputStream* st) {
  st->print(" value=");
  print_symbol_on(st);
}

// ------------------------------------------------------------------
// ciSymbol::print_symbol_on
//
// Print the value of this symbol on an outputStream
void ciSymbol::print_symbol_on(outputStream *st) {
  GUARDED_VM_ENTRY(get_symbol()->print_symbol_on(st);)
}

const char* ciSymbol::as_klass_external_name() const {
  GUARDED_VM_ENTRY(return get_symbol()->as_klass_external_name(););
}

// ------------------------------------------------------------------
// ciSymbol::make_impl
//
// Make a ciSymbol from a C string (implementation).
ciSymbol* ciSymbol::make_impl(const char* s) {
  EXCEPTION_CONTEXT;
  TempNewSymbol sym = SymbolTable::new_symbol(s, THREAD);
  if (HAS_PENDING_EXCEPTION) {
    CLEAR_PENDING_EXCEPTION;
    CURRENT_THREAD_ENV->record_out_of_memory_failure();
    return ciEnv::_unloaded_cisymbol;
  }
  return CURRENT_THREAD_ENV->get_symbol(sym);
}

// ------------------------------------------------------------------
// ciSymbol::make
//
// Make a ciSymbol from a C string.
ciSymbol* ciSymbol::make(const char* s) {
  GUARDED_VM_ENTRY(return make_impl(s);)
}
