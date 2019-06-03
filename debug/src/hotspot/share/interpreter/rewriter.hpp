#ifndef SHARE_VM_INTERPRETER_REWRITER_HPP
#define SHARE_VM_INTERPRETER_REWRITER_HPP

#include "memory/allocation.hpp"
#include "utilities/growableArray.hpp"

// The Rewriter adds caches to the constant pool and rewrites bytecode indices
// pointing into the constant pool for better interpreter performance.

class Rewriter: public StackObj {
 private:
  InstanceKlass*      _klass;
  constantPoolHandle  _pool;
  Array<Method*>*     _methods;
  GrowableArray<int>  _cp_map;
  GrowableArray<int>  _cp_cache_map;  // for Methodref, Fieldref,
                                      // InterfaceMethodref and InvokeDynamic
  GrowableArray<int>  _reference_map; // maps from cp index to resolved_refs index (or -1)
  GrowableArray<int>  _resolved_references_map; // for strings, methodHandle, methodType
  GrowableArray<int>  _invokedynamic_references_map; // for invokedynamic resolved refs
  GrowableArray<int>  _method_handle_invokers;
  int                 _resolved_reference_limit;

  // For mapping invokedynamic bytecodes, which are discovered during method
  // scanning.  The invokedynamic entries are added at the end of the cpCache.
  // If there are any invokespecial/InterfaceMethodref special case bytecodes,
  // these entries are added before invokedynamic entries so that the
  // invokespecial bytecode 16 bit index doesn't overflow.
  GrowableArray<int>      _invokedynamic_cp_cache_map;

  // For patching.
  GrowableArray<address>* _patch_invokedynamic_bcps;
  GrowableArray<int>*     _patch_invokedynamic_refs;

  void init_maps(int length) {
    _cp_map.trunc_to(0);
    _cp_map.at_grow(length, -1);

    _cp_cache_map.trunc_to(0);
    // Also cache resolved objects, in another different cache.
    _reference_map.trunc_to(0);
    _reference_map.at_grow(length, -1);

    _method_handle_invokers.trunc_to(0);
    _resolved_references_map.trunc_to(0);
    _invokedynamic_references_map.trunc_to(0);
    _resolved_reference_limit = -1;
    _first_iteration_cp_cache_limit = -1;

    // invokedynamic specific fields
    _invokedynamic_cp_cache_map.trunc_to(0);
    _patch_invokedynamic_bcps = new GrowableArray<address>(length / 4);
    _patch_invokedynamic_refs = new GrowableArray<int>(length / 4);
  }

  int _first_iteration_cp_cache_limit;
  void record_map_limits() {
    // Record initial size of the two arrays generated for the CP cache
    // relative to walking the constant pool.
    _first_iteration_cp_cache_limit = _cp_cache_map.length();
    _resolved_reference_limit = _resolved_references_map.length();
  }

  int cp_cache_delta() {
    return _cp_cache_map.length() - _first_iteration_cp_cache_limit;
  }

  int  cp_entry_to_cp_cache(int i) { return _cp_map.at(i); }
  bool has_cp_cache(int i) { return (uint) i < (uint) _cp_map.length() && _cp_map.at(i) >= 0; }

  int add_map_entry(int cp_index, GrowableArray<int>* cp_map, GrowableArray<int>* cp_cache_map) {
    int cache_index = cp_cache_map->append(cp_index);
    cp_map->at_put(cp_index, cache_index);
    return cache_index;
  }

  int add_cp_cache_entry(int cp_index) {
    int cache_index = add_map_entry(cp_index, &_cp_map, &_cp_cache_map);
    return cache_index;
  }

  int add_invokedynamic_cp_cache_entry(int cp_index) {
    // add to the invokedynamic index map.
    int cache_index = _invokedynamic_cp_cache_map.append(cp_index);
    // this index starts at one but in the bytecode it's appended to the end.
    return cache_index + _first_iteration_cp_cache_limit;
  }

  int invokedynamic_cp_cache_entry_pool_index(int cache_index) {
    int cp_index = _invokedynamic_cp_cache_map.at(cache_index);
    return cp_index;
  }

  // add a new CP cache entry beyond the normal cache for the special case of
  // invokespecial with InterfaceMethodref as cpool operand.
  int add_invokespecial_cp_cache_entry(int cp_index) {
    // Don't add InterfaceMethodref if it already exists at the end.
    for (int i = _first_iteration_cp_cache_limit; i < _cp_cache_map.length(); i++) {
      if (cp_cache_entry_pool_index(i) == cp_index) {
        return i;
      }
    }
    int cache_index = _cp_cache_map.append(cp_index);
    return cache_index;
  }

  int cp_entry_to_resolved_references(int cp_index) const {
    return _reference_map.at(cp_index);
  }
  bool has_entry_in_resolved_references(int cp_index) const {
    return (uint) cp_index < (uint) _reference_map.length() && _reference_map.at(cp_index) >= 0;
  }

  // add a new entry to the resolved_references map
  int add_resolved_references_entry(int cp_index) {
    int ref_index = add_map_entry(cp_index, &_reference_map, &_resolved_references_map);
    return ref_index;
  }

  // add a new entries to the resolved_references map (for invokedynamic and invokehandle only)
  int add_invokedynamic_resolved_references_entries(int cp_index, int cache_index) {
    int ref_index = -1;
    for (int entry = 0; entry < ConstantPoolCacheEntry::_indy_resolved_references_entries; entry++) {
      const int index = _resolved_references_map.append(cp_index);  // many-to-one
      if (entry == 0) {
        ref_index = index;
      }
      _invokedynamic_references_map.at_put_grow(index, cache_index, -1);
    }
    return ref_index;
  }

  int resolved_references_entry_to_pool_index(int ref_index) {
    int cp_index = _resolved_references_map.at(ref_index);
    return cp_index;
  }

  // Access the contents of _cp_cache_map to determine CP cache layout.
  int cp_cache_entry_pool_index(int cache_index) {
    int cp_index = _cp_cache_map.at(cache_index);
    return cp_index;
  }

  // All the work goes in here:
  Rewriter(InstanceKlass* klass, const constantPoolHandle& cpool, Array<Method*>* methods, TRAPS);

  void compute_index_maps();
  void make_constant_pool_cache(TRAPS);
  void scan_method(Method* m, bool reverse, bool* invokespecial_error);
  void rewrite_Object_init(const methodHandle& m, TRAPS);
  void rewrite_member_reference(address bcp, int offset, bool reverse);
  void maybe_rewrite_invokehandle(address opc, int cp_index, int cache_index, bool reverse);
  void rewrite_invokedynamic(address bcp, int offset, bool reverse);
  void maybe_rewrite_ldc(address bcp, int offset, bool is_wide, bool reverse);
  void rewrite_invokespecial(address bcp, int offset, bool reverse, bool* invokespecial_error);

  void patch_invokedynamic_bytecodes();

  // Do all the work.
  void rewrite_bytecodes(TRAPS);

  // Revert bytecodes in case of an exception.
  void restore_bytecodes();

  static methodHandle rewrite_jsrs(const methodHandle& m, TRAPS);
 public:
  // Driver routine:
  static void rewrite(InstanceKlass* klass, TRAPS);
};

#endif
