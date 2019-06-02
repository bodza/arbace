#include "precompiled.hpp"

#include "code/vtableStubs.hpp"
#include "compiler/compileBroker.hpp"
#include "compiler/disassembler.hpp"
#include "logging/log.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/klassVtable.hpp"
#include "oops/oop.inline.hpp"
#include "prims/forte.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/sharedRuntime.hpp"
#include "utilities/align.hpp"

// -----------------------------------------------------------------------------------------
// Implementation of VtableStub

address VtableStub::_chunk             = NULL;
address VtableStub::_chunk_end         = NULL;
VMReg   VtableStub::_receiver_location = VMRegImpl::Bad();

void* VtableStub::operator new(size_t size, int code_size) throw() {
  // compute real VtableStub size (rounded to nearest word)
  const int real_size = align_up(code_size + (int)sizeof(VtableStub), wordSize);
  // malloc them in chunks to minimize header overhead
  const int chunk_factor = 32;
  if (_chunk == NULL || _chunk + real_size > _chunk_end) {
    const int bytes = chunk_factor * real_size + pd_code_alignment();

   // There is a dependency on the name of the blob in src/share/vm/prims/jvmtiCodeBlobEvents.cpp
   // If changing the name, update the other file accordingly.
    VtableBlob* blob = VtableBlob::create("vtable chunks", bytes);
    if (blob == NULL) {
      return NULL;
    }
    _chunk = blob->content_begin();
    _chunk_end = _chunk + bytes;
    Forte::register_stub("vtable stub", _chunk, _chunk_end);
    align_chunk();
  }
  void* res = _chunk;
  _chunk += real_size;
  align_chunk();
 return res;
}

void VtableStub::print_on(outputStream* st) const {
  st->print("vtable stub (index = %d, receiver_location = " INTX_FORMAT ", code = [" INTPTR_FORMAT ", " INTPTR_FORMAT "[)",
             index(), p2i(receiver_location()), p2i(code_begin()), p2i(code_end()));
}

// -----------------------------------------------------------------------------------------
// Implementation of VtableStubs
//
// For each hash value there's a linked list of vtable stubs (with that
// hash value). Each list is anchored in a little hash _table, indexed
// by that hash value.

VtableStub* VtableStubs::_table[VtableStubs::N];
int VtableStubs::_number_of_vtable_stubs = 0;
int VtableStubs::_vtab_stub_size = 0;
int VtableStubs::_itab_stub_size = 0;

// These values are good for the PRODUCT case (no tracing).
static const int first_vtableStub_size =  64;
static const int first_itableStub_size = 256;

void VtableStubs::initialize() {
  VtableStub::_receiver_location = SharedRuntime::name_for_receiver();
  {
    MutexLocker ml(VtableStubs_lock);
    for (int i = 0; i < N; i++) {
      _table[i] = NULL;
    }
  }
}

int VtableStubs::code_size_limit(bool is_vtable_stub) {
  if (is_vtable_stub) {
    return _vtab_stub_size > 0 ? _vtab_stub_size : first_vtableStub_size;
  } else { // itable stub
    return _itab_stub_size > 0 ? _itab_stub_size : first_itableStub_size;
  }
}   // code_size_limit

void VtableStubs::check_and_set_size_limit(bool is_vtable_stub, int code_size, int padding) {
  const char* name = is_vtable_stub ? "vtable" : "itable";

  guarantee(code_size <= code_size_limit(is_vtable_stub), "buffer overflow in %s stub, code_size is %d, limit is %d", name, code_size, code_size_limit(is_vtable_stub));

  if (is_vtable_stub) {
    if ((code_size + padding) > _vtab_stub_size ) {
      _vtab_stub_size = code_size + padding;
    }
  } else {  // itable stub
    if ((code_size + padding) > _itab_stub_size ) {
      _itab_stub_size = code_size + padding;
    }
  }
  return;
}   // check_and_set_size_limit

void VtableStubs::bookkeeping(MacroAssembler* masm, outputStream* out, VtableStub* s, address npe_addr, address ame_addr, bool is_vtable_stub, int index, int slop_bytes, int index_dependent_slop) {
  const char* name        = is_vtable_stub ? "vtable" : "itable";
  const int   stub_length = code_size_limit(is_vtable_stub);
  guarantee(masm->pc() <= s->code_end(), "%s #%d: overflowed buffer, estimated len: %d, actual len: %d, overrun: %d",
                                         name, index, stub_length,
                                         (int)(masm->pc() - s->code_begin()),
                                         (int)(masm->pc() - s->code_end()));

  // After the first vtable/itable stub is generated, we have a much
  // better estimate for the stub size. Remember/update this
  // estimate after some sanity checks.
  check_and_set_size_limit(is_vtable_stub, masm->offset(), slop_bytes);
  s->set_exception_points(npe_addr, ame_addr);
}

address VtableStubs::find_stub(bool is_vtable_stub, int vtable_index) {

  VtableStub* s = ShareVtableStubs ? lookup(is_vtable_stub, vtable_index) : NULL;
  if (s == NULL) {
    if (is_vtable_stub) {
      s = create_vtable_stub(vtable_index);
    } else {
      s = create_itable_stub(vtable_index);
    }

    // Creation of vtable or itable can fail if there is not enough free space in the code cache.
    if (s == NULL) {
      return NULL;
    }

    enter(is_vtable_stub, vtable_index, s);
  }
  return s->entry_point();
}

inline uint VtableStubs::hash(bool is_vtable_stub, int vtable_index) {
  // Assumption: receiver_location < 4 in most cases.
  int hash = ((vtable_index << 2) ^ VtableStub::receiver_location()->value()) + vtable_index;
  return (is_vtable_stub ? ~hash : hash)  & mask;
}

VtableStub* VtableStubs::lookup(bool is_vtable_stub, int vtable_index) {
  MutexLocker ml(VtableStubs_lock);
  unsigned hash = VtableStubs::hash(is_vtable_stub, vtable_index);
  VtableStub* s = _table[hash];
  while ( s && !s->matches(is_vtable_stub, vtable_index)) s = s->next();
  return s;
}

void VtableStubs::enter(bool is_vtable_stub, int vtable_index, VtableStub* s) {
  MutexLocker ml(VtableStubs_lock);
  unsigned int h = VtableStubs::hash(is_vtable_stub, vtable_index);
  // enter s at the beginning of the corresponding list
  s->set_next(_table[h]);
  _table[h] = s;
  _number_of_vtable_stubs++;
}

VtableStub* VtableStubs::entry_point(address pc) {
  MutexLocker ml(VtableStubs_lock);
  VtableStub* stub = (VtableStub*)(pc - VtableStub::entry_offset());
  uint hash = VtableStubs::hash(stub->is_vtable_stub(), stub->index());
  VtableStub* s;
  for (s = _table[hash]; s != NULL && s != stub; s = s->next()) { }
  return (s == stub) ? s : NULL;
}

bool VtableStubs::contains(address pc) {
  // simple solution for now - we may want to use
  // a faster way if this function is called often
  return stub_containing(pc) != NULL;
}

VtableStub* VtableStubs::stub_containing(address pc) {
  // Note: No locking needed since any change to the data structure
  //       happens with an atomic store into it (we don't care about
  //       consistency with the _number_of_vtable_stubs counter).
  for (int i = 0; i < N; i++) {
    for (VtableStub* s = _table[i]; s != NULL; s = s->next()) {
      if (s->contains(pc)) return s;
    }
  }
  return NULL;
}

void vtableStubs_init() {
  VtableStubs::initialize();
}

void VtableStubs::vtable_stub_do(void f(VtableStub*)) {
    for (int i = 0; i < N; i++) {
        for (VtableStub* s = _table[i]; s != NULL; s = s->next()) {
            f(s);
        }
    }
}
