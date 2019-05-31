#ifndef SHARE_VM_OOPS_METADATA_HPP
#define SHARE_VM_OOPS_METADATA_HPP

#include "utilities/exceptions.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/ostream.hpp"

// This is the base class for an internal Class related metadata
class Metadata : public MetaspaceObj {
 public:
  int identity_hash()                { return (int)(uintptr_t)this; }

  // Rehashing support for tables containing pointers to this
  unsigned int new_hash(juint seed)   { ShouldNotReachHere();  return 0; }

  virtual bool is_metadata()           const volatile { return true; }
  virtual bool is_klass()              const volatile { return false; }
  virtual bool is_method()             const volatile { return false; }
  virtual bool is_methodData()         const volatile { return false; }
  virtual bool is_constantPool()       const volatile { return false; }
  virtual bool is_methodCounters()     const volatile { return false; }
  virtual int  size()                  const = 0;
  virtual MetaspaceObj::Type type()    const = 0;
  virtual const char* internal_name()  const = 0;
  virtual void metaspace_pointers_do(MetaspaceClosure* iter) { }

  void print()       const { print_on(tty); }
  void print_value() const { print_value_on(tty); }

  void print_maybe_null() const { print_on_maybe_null(tty); }
  void print_on_maybe_null(outputStream* st) const {
    if (this == NULL)
      st->print("NULL");
    else
      print_on(st);
  }
  void print_value_on_maybe_null(outputStream* st) const {
    if (this == NULL)
      st->print("NULL");
    else
      print_value_on(st);
  }

  virtual void print_on(outputStream* st) const;       // First level print
  virtual void print_value_on(outputStream* st) const = 0; // Second level print

  char* print_value_string() const;

  // Used to keep metadata alive during class redefinition
  // Can't assert because is called for delete functions (as an assert)
  virtual bool on_stack() const { return false; }
  virtual void set_on_stack(const bool value);

  // Set on_stack bit, so that the metadata is not cleared
  // during class redefinition.  This is a virtual call because only methods
  // and constant pools need to be set, but someday instanceKlasses might also.
  static void mark_on_stack(Metadata* m) { m->set_on_stack(true); }
};

#endif
