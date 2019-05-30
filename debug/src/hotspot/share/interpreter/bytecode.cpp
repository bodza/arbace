#include "precompiled.hpp"
#include "interpreter/bytecode.inline.hpp"
#include "interpreter/linkResolver.hpp"
#include "oops/constantPool.hpp"
#include "oops/cpCache.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/fieldType.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/safepoint.hpp"
#include "runtime/signature.hpp"

// Implementation of Bytecode

// Implementation of Bytecode_tableupswitch

int Bytecode_tableswitch::dest_offset_at(int i) const {
  return get_aligned_Java_u4_at(1 + (3 + i)*jintSize);
}

// Implementation of Bytecode_invoke

void Bytecode_invoke::verify() const {
  assert(is_valid(), "check invoke");
  assert(cpcache() != NULL, "do not call this from verifier or rewriter");
}

int Bytecode_invoke::size_of_parameters() const {
  ArgumentSizeComputer asc(signature());
  return asc.size() + (has_receiver() ? 1 : 0);
}

Symbol* Bytecode_member_ref::klass() const {
  return constants()->klass_ref_at_noresolve(index());
}

Symbol* Bytecode_member_ref::name() const {
  return constants()->name_ref_at(index());
}

Symbol* Bytecode_member_ref::signature() const {
  return constants()->signature_ref_at(index());
}

BasicType Bytecode_member_ref::result_type() const {
  ResultTypeFinder rts(signature());
  rts.iterate();
  return rts.type();
}

methodHandle Bytecode_invoke::static_target(TRAPS) {
  constantPoolHandle constants(THREAD, this->constants());

  Bytecodes::Code bc = invoke_code();
  return LinkResolver::resolve_method_statically(bc, constants, index(), THREAD);
}

Handle Bytecode_invoke::appendix(TRAPS) {
  ConstantPoolCacheEntry* cpce = cpcache_entry();
  if (cpce->has_appendix())
    return Handle(THREAD, cpce->appendix_if_resolved(constants()));
  return Handle();  // usual case
}

int Bytecode_member_ref::index() const {
  // Note:  Rewriter::rewrite changes the Java_u2 of an invokedynamic to a native_u4,
  // at the same time it allocates per-call-site CP cache entries.
  Bytecodes::Code rawc = code();
  if (has_index_u4(rawc))
    return get_index_u4(rawc);
  else
    return get_index_u2_cpcache(rawc);
}

int Bytecode_member_ref::pool_index() const {
  return cpcache_entry()->constant_pool_index();
}

ConstantPoolCacheEntry* Bytecode_member_ref::cpcache_entry() const {
  int index = this->index();
  return cpcache()->entry_at(ConstantPool::decode_cpcache_index(index, true));
}

// Implementation of Bytecode_field

void Bytecode_field::verify() const {
  assert(is_valid(), "check field");
}

// Implementation of Bytecode_loadconstant

int Bytecode_loadconstant::raw_index() const {
  Bytecodes::Code rawc = code();
  assert(rawc != Bytecodes::_wide, "verifier prevents this");
  if (Bytecodes::java_code(rawc) == Bytecodes::_ldc)
    return get_index_u1(rawc);
  else
    return get_index_u2(rawc, false);
}

int Bytecode_loadconstant::pool_index() const {
  int index = raw_index();
  if (has_cache_index()) {
    return _method->constants()->object_to_cp_index(index);
  }
  return index;
}

BasicType Bytecode_loadconstant::result_type() const {
  int index = pool_index();
  return _method->constants()->basic_type_for_constant_at(index);
}

oop Bytecode_loadconstant::resolve_constant(TRAPS) const {
  assert(_method != NULL, "must supply method to resolve constant");
  int index = raw_index();
  ConstantPool* constants = _method->constants();
  if (has_cache_index()) {
    return constants->resolve_cached_constant_at(index, THREAD);
  } else if (_method->constants()->tag_at(index).is_dynamic_constant()) {
    return constants->resolve_possibly_cached_constant_at(index, THREAD);
  } else {
    return constants->resolve_constant_at(index, THREAD);
  }
}
