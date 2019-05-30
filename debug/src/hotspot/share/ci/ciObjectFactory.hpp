#ifndef SHARE_VM_CI_CIOBJECTFACTORY_HPP
#define SHARE_VM_CI_CIOBJECTFACTORY_HPP

#include "ci/ciClassList.hpp"
#include "ci/ciObject.hpp"
#include "utilities/growableArray.hpp"

// ciObjectFactory
//
// This class handles requests for the creation of new instances
// of ciObject and its subclasses.  It contains a caching mechanism
// which ensures that for each oop, at most one ciObject is created.
// This invariant allows efficient implementation of ciObject.
class ciObjectFactory : public ResourceObj {
  friend class VMStructs;
  friend class ciEnv;

private:
  static volatile bool _initialized;
  static GrowableArray<ciMetadata*>* _shared_ci_metadata;
  static ciSymbol*                 _shared_ci_symbols[];
  static int                       _shared_ident_limit;

  Arena*                    _arena;
  GrowableArray<ciMetadata*>*        _ci_metadata;
  GrowableArray<ciMethod*>* _unloaded_methods;
  GrowableArray<ciKlass*>* _unloaded_klasses;
  GrowableArray<ciInstance*>* _unloaded_instances;
  GrowableArray<ciReturnAddress*>* _return_addresses;
  GrowableArray<ciSymbol*>* _symbols;  // keep list of symbols created
  int                       _next_ident;

public:
  struct NonPermObject : public ResourceObj {
    ciObject*      _object;
    NonPermObject* _next;

    inline NonPermObject(NonPermObject* &bucket, oop key, ciObject* object);
    ciObject*     object()  { return _object; }
    NonPermObject* &next()  { return _next; }
  };
private:
  enum { NON_PERM_BUCKETS = 61 };
  NonPermObject* _non_perm_bucket[NON_PERM_BUCKETS];
  int _non_perm_count;

  static int metadata_compare(Metadata* const& key, ciMetadata* const& elt);

  ciObject* create_new_object(oop o);
  ciMetadata* create_new_metadata(Metadata* o);

  static bool is_equal(NonPermObject* p, oop key) {
    return p->object()->get_oop() == key;
  }

  NonPermObject* &find_non_perm(oop key);
  void insert_non_perm(NonPermObject* &where, oop key, ciObject* obj);

  void init_ident_of(ciBaseObject* obj);

  Arena* arena() { return _arena; }

  void print_contents_impl();

  ciInstance* get_unloaded_instance(ciInstanceKlass* klass);

public:
  static bool is_initialized() { return _initialized; }

  static void initialize();
  void init_shared_objects();
  void remove_symbols();

  ciObjectFactory(Arena* arena, int expected_size);

  // Get the ciObject corresponding to some oop.
  ciObject* get(oop key);
  ciMetadata* get_metadata(Metadata* key);
  ciSymbol* get_symbol(Symbol* key);

  // Get the ciSymbol corresponding to one of the vmSymbols.
  static ciSymbol* vm_symbol_at(int index);

  // Get the ciMethod representing an unloaded/unfound method.
  ciMethod* get_unloaded_method(ciInstanceKlass* holder,
                                ciSymbol*        name,
                                ciSymbol*        signature,
                                ciInstanceKlass* accessor);

  // Get a ciKlass representing an unloaded klass.
  ciKlass* get_unloaded_klass(ciKlass* accessing_klass,
                              ciSymbol* name,
                              bool create_if_not_found);

  // Get a ciInstance representing an unresolved klass mirror.
  ciInstance* get_unloaded_klass_mirror(ciKlass* type);

  // Get a ciInstance representing an unresolved method handle constant.
  ciInstance* get_unloaded_method_handle_constant(ciKlass*  holder,
                                                  ciSymbol* name,
                                                  ciSymbol* signature,
                                                  int       ref_kind);

  // Get a ciInstance representing an unresolved method type constant.
  ciInstance* get_unloaded_method_type_constant(ciSymbol* signature);

  ciInstance* get_unloaded_object_constant();

  // Get the ciMethodData representing the methodData for a method
  // with none.
  ciMethodData* get_empty_methodData();

  ciReturnAddress* get_return_address(int bci);

  GrowableArray<ciMetadata*>* get_ci_metadata() const { return _ci_metadata; }
  // RedefineClasses support
  void metadata_do(void f(Metadata*));

  void print_contents();
  void print();
};

#endif
