#include "precompiled.hpp"

#include "code/codeBlob.hpp"
#include "code/codeCache.hpp"
#include "code/nmethod.hpp"
#include "code/scopeDesc.hpp"
#include "compiler/oopMap.hpp"
#include "gc/shared/collectedHeap.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/iterator.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/signature.hpp"
#include "utilities/align.hpp"
#include "c1/c1_Defs.hpp"

// OopMapStream

OopMapStream::OopMapStream(OopMap* oop_map, int oop_types_mask) {
  _stream = new CompressedReadStream(oop_map->write_stream()->buffer());
  _mask = oop_types_mask;
  _size = oop_map->omv_count();
  _position = 0;
  _valid_omv = false;
}

OopMapStream::OopMapStream(const ImmutableOopMap* oop_map, int oop_types_mask) {
  _stream = new CompressedReadStream(oop_map->data_addr());
  _mask = oop_types_mask;
  _size = oop_map->count();
  _position = 0;
  _valid_omv = false;
}

void OopMapStream::find_next() {
  while (_position++ < _size) {
    _omv.read_from(_stream);
    if (((int)_omv.type() & _mask) > 0) {
      _valid_omv = true;
      return;
    }
  }
  _valid_omv = false;
}

// OopMap

// frame_size units are stack-slots (4 bytes) NOT intptr_t; we can name odd
// slots to hold 4-byte values like ints and floats in the LP64 build.
OopMap::OopMap(int frame_size, int arg_count) {
  // OopMaps are usually quite so small, so pick a small initial size
  set_write_stream(new CompressedWriteStream(32));
  set_omv_count(0);
}

OopMap::OopMap(OopMap::DeepCopyToken, OopMap* source) {
  // This constructor does a deep copy
  // of the source OopMap.
  set_write_stream(new CompressedWriteStream(source->omv_count() * 2));
  set_omv_count(0);
  set_offset(source->offset());

  // We need to copy the entries too.
  for (OopMapStream oms(source); !oms.is_done(); oms.next()) {
    OopMapValue omv = oms.current();
    omv.write_on(write_stream());
    increment_count();
  }
}

OopMap* OopMap::deep_copy() {
  return new OopMap(_deep_copy_token, this);
}

void OopMap::copy_data_to(address addr) const {
  memcpy(addr, write_stream()->buffer(), write_stream()->position());
}

int OopMap::heap_size() const {
  int size = sizeof(OopMap);
  int align = sizeof(void *) - 1;
  size += write_stream()->position();
  // Align to a reasonable ending point
  size = ((size+align) & ~align);
  return size;
}

// frame_size units are stack-slots (4 bytes) NOT intptr_t; we can name odd
// slots to hold 4-byte values like ints and floats in the LP64 build.
void OopMap::set_xxx(VMReg reg, OopMapValue::oop_types x, VMReg optional) {
  OopMapValue o(reg, x);

  if (x == OopMapValue::callee_saved_value) {
    // This can never be a stack location, so we don't need to transform it.
    o.set_content_reg(optional);
  } else if (x == OopMapValue::derived_oop_value) {
    o.set_content_reg(optional);
  }

  o.write_on(write_stream());
  increment_count();
}

void OopMap::set_oop(VMReg reg) {
  set_xxx(reg, OopMapValue::oop_value, VMRegImpl::Bad());
}

void OopMap::set_value(VMReg reg) {
  // At this time, we don't need value entries in our OopMap.
}

void OopMap::set_narrowoop(VMReg reg) {
  set_xxx(reg, OopMapValue::narrowoop_value, VMRegImpl::Bad());
}

void OopMap::set_callee_saved(VMReg reg, VMReg caller_machine_register ) {
  set_xxx(reg, OopMapValue::callee_saved_value, caller_machine_register);
}

void OopMap::set_derived_oop(VMReg reg, VMReg derived_from_local_register ) {
  if (reg == derived_from_local_register ) {
    // Actually an oop, derived shares storage with base,
    set_oop(reg);
  } else {
    set_xxx(reg, OopMapValue::derived_oop_value, derived_from_local_register);
  }
}

// OopMapSet

OopMapSet::OopMapSet() {
  set_om_size(MinOopMapAllocation);
  set_om_count(0);
  OopMap** temp = NEW_RESOURCE_ARRAY(OopMap*, om_size());
  set_om_data(temp);
}

void OopMapSet::grow_om_data() {
  int new_size = om_size() * 2;
  OopMap** new_data = NEW_RESOURCE_ARRAY(OopMap*, new_size);
  memcpy(new_data,om_data(),om_size() * sizeof(OopMap*));
  set_om_size(new_size);
  set_om_data(new_data);
}

void OopMapSet::add_gc_map(int pc_offset, OopMap *map ) {
  if (om_count() >= om_size()) {
    grow_om_data();
  }
  map->set_offset(pc_offset);

  set(om_count(),map);
  increment_count();
}

int OopMapSet::heap_size() const {
  // The space we use
  int size = sizeof(OopMap);
  int align = sizeof(void *) - 1;
  size = ((size+align) & ~align);
  size += om_count() * sizeof(OopMap*);

  // Now add in the space needed for the indivdiual OopMaps
  for (int i = 0; i < om_count(); i++) {
    size += at(i)->heap_size();
  }
  // We don't need to align this, it will be naturally pointer aligned
  return size;
}

OopMap* OopMapSet::singular_oop_map() {
  guarantee(om_count() == 1, "Make sure we only have a single gc point");
  return at(0);
}

OopMap* OopMapSet::find_map_at_offset(int pc_offset) const {
  int i, len = om_count();

  // Scan through oopmaps. Stop when current offset is either equal or greater
  // than the one we are looking for.
  for ( i = 0; i < len; i++) {
    if (at(i)->offset() >= pc_offset )
      break;
  }

  OopMap* m = at(i);
  return m;
}

static void add_derived_oop(oop* base, oop* derived) {
  DerivedPointerTable::add(derived, base);
}

void OopMapSet::oops_do(const frame *fr, const RegisterMap* reg_map, OopClosure* f) {
  // add derived oops to a table
  all_do(fr, reg_map, f, add_derived_oop, &do_nothing_cl);
}

void OopMapSet::all_do(const frame *fr, const RegisterMap *reg_map, OopClosure* oop_fn, void derived_oop_fn(oop*, oop*), OopClosure* value_fn) {
  CodeBlob* cb = fr->cb();

  const ImmutableOopMapSet* maps = cb->oop_maps();
  const ImmutableOopMap* map = cb->oop_map_for_return_address(fr->pc());

  // handle derived pointers first (otherwise base pointer may be
  // changed before derived pointer offset has been collected)
  OopMapValue omv;
  {
    OopMapStream oms(map,OopMapValue::derived_oop_value);
    if (!oms.is_done()) {
      ShouldNotReachHere();
    }
  }

  // We want coop and oop oop_types
  int mask = OopMapValue::oop_value | OopMapValue::narrowoop_value;
  {
    for (OopMapStream oms(map,mask); !oms.is_done(); oms.next()) {
      omv = oms.current();
      oop* loc = fr->oopmapreg_to_location(omv.reg(),reg_map);
      // It should be an error if no location can be found for a
      // register mentioned as contained an oop of some kind.  Maybe
      // this was allowed previously because value_value items might
      // be missing?
      guarantee(loc != NULL, "missing saved register");
      if (omv.type() == OopMapValue::oop_value ) {
        oop val = *loc;
        if (val == (oop)NULL || Universe::is_narrow_oop_base(val)) {
          // Ignore NULL oops and decoded NULL narrow oops which
          // equal to Universe::narrow_oop_base when a narrow oop
          // implicit null check is used in compiled code.
          // The narrow_oop_base could be NULL or be the address
          // of the page below heap depending on compressed oops mode.
          continue;
        }
        oop_fn->do_oop(loc);
      } else if (omv.type() == OopMapValue::narrowoop_value ) {
        narrowOop *nl = (narrowOop*)loc;
#ifndef VM_LITTLE_ENDIAN
        VMReg vmReg = omv.reg();
        // Don't do this on SPARC float registers as they can be individually addressed
        if (!vmReg->is_stack()) {
          // compressed oops in registers only take up 4 bytes of an
          // 8 byte register but they are in the wrong part of the
          // word so adjust loc to point at the right place.
          nl = (narrowOop*)((address)nl + 4);
        }
#endif
        oop_fn->do_oop(nl);
      }
    }
  }
}

// Update callee-saved register info for the following frame
void OopMapSet::update_register_map(const frame *fr, RegisterMap *reg_map) {
  ResourceMark rm;
  CodeBlob* cb = fr->cb();

  // Scan through oopmap and find location of all callee-saved registers
  // (we do not do update in place, since info could be overwritten)

  address pc = fr->pc();
  const ImmutableOopMap* map  = cb->oop_map_for_return_address(pc);

  for (OopMapStream oms(map, OopMapValue::callee_saved_value); !oms.is_done(); oms.next()) {
    OopMapValue omv = oms.current();
    VMReg reg = omv.content_reg();
    oop* loc = fr->oopmapreg_to_location(omv.reg(), reg_map);
    reg_map->set_location(reg, (address) loc);
  }

  // Check that runtime stubs save all callee-saved registers
}

// Printing code is present in product build for -XX:+PrintAssembly.

static
void print_register_type(OopMapValue::oop_types x, VMReg optional, outputStream* st) {
  switch( x ) {
  case OopMapValue::oop_value:
    st->print("Oop");
    break;
  case OopMapValue::narrowoop_value:
    st->print("NarrowOop");
    break;
  case OopMapValue::callee_saved_value:
    st->print("Callers_");
    optional->print_on(st);
    break;
  case OopMapValue::derived_oop_value:
    st->print("Derived_oop_");
    optional->print_on(st);
    break;
  default:
    ShouldNotReachHere();
  }
}

void OopMapValue::print_on(outputStream* st) const {
  reg()->print_on(st);
  st->print("=");
  print_register_type(type(),content_reg(),st);
  st->print(" ");
}

void ImmutableOopMap::print_on(outputStream* st) const {
  OopMapValue omv;
  st->print("ImmutableOopMap{");
  for (OopMapStream oms(this); !oms.is_done(); oms.next()) {
    omv = oms.current();
    omv.print_on(st);
  }
  st->print("}");
}

void OopMap::print_on(outputStream* st) const {
  OopMapValue omv;
  st->print("OopMap{");
  for (OopMapStream oms((OopMap*)this); !oms.is_done(); oms.next()) {
    omv = oms.current();
    omv.print_on(st);
  }
  st->print("off=%d}", (int) offset());
}

void ImmutableOopMapSet::print_on(outputStream* st) const {
  const ImmutableOopMap* last = NULL;
  for (int i = 0; i < _count; ++i) {
    const ImmutableOopMapPair* pair = pair_at(i);
    const ImmutableOopMap* map = pair->get_from(this);
    if (map != last) {
      st->cr();
      map->print_on(st);
      st->print("pc offsets: ");
    }
    last = map;
    st->print("%d ", pair->pc_offset());
  }
}

void OopMapSet::print_on(outputStream* st) const {
  int i, len = om_count();

  st->print_cr("OopMapSet contains %d OopMaps\n",len);

  for ( i = 0; i < len; i++) {
    OopMap* m = at(i);
    st->print_cr("#%d ",i);
    m->print_on(st);
    st->cr();
  }
}

bool OopMap::equals(const OopMap* other) const {
  if (other->_omv_count != _omv_count) {
    return false;
  }
  if (other->write_stream()->position() != write_stream()->position()) {
    return false;
  }
  if (memcmp(other->write_stream()->buffer(), write_stream()->buffer(), write_stream()->position()) != 0) {
    return false;
  }
  return true;
}

const ImmutableOopMap* ImmutableOopMapSet::find_map_at_offset(int pc_offset) const {
  ImmutableOopMapPair* pairs = get_pairs();

  int i;
  for (i = 0; i < _count; ++i) {
    if (pairs[i].pc_offset() >= pc_offset) {
      break;
    }
  }
  ImmutableOopMapPair* last = &pairs[i];

  return last->get_from(this);
}

const ImmutableOopMap* ImmutableOopMapPair::get_from(const ImmutableOopMapSet* set) const {
  return set->oopmap_at_offset(_oopmap_offset);
}

ImmutableOopMap::ImmutableOopMap(const OopMap* oopmap) : _count(oopmap->count()) {
  address addr = data_addr();
  oopmap->copy_data_to(addr);
}

ImmutableOopMapBuilder::ImmutableOopMapBuilder(const OopMapSet* set) : _set(set), _new_set(NULL), _empty(NULL), _last(NULL), _empty_offset(-1), _last_offset(-1), _offset(0), _required(-1) {
  _mapping = NEW_RESOURCE_ARRAY(Mapping, _set->size());
}

int ImmutableOopMapBuilder::size_for(const OopMap* map) const {
  return align_up((int)sizeof(ImmutableOopMap) + map->data_size(), 8);
}

int ImmutableOopMapBuilder::heap_size() {
  int base = sizeof(ImmutableOopMapSet);
  base = align_up(base, 8);

  // all of ours pc / offset pairs
  int pairs = _set->size() * sizeof(ImmutableOopMapPair);
  pairs = align_up(pairs, 8);

  for (int i = 0; i < _set->size(); ++i) {
    int size = 0;
    OopMap* map = _set->at(i);

    if (is_empty(map)) {
      /* only keep a single empty map in the set */
      if (has_empty()) {
        _mapping[i].set(Mapping::OOPMAP_EMPTY, _empty_offset, 0, map, _empty);
      } else {
        _empty_offset = _offset;
        _empty = map;
        size = size_for(map);
        _mapping[i].set(Mapping::OOPMAP_NEW, _offset, size, map);
      }
    } else if (is_last_duplicate(map)) {
      /* if this entry is identical to the previous one, just point it there */
      _mapping[i].set(Mapping::OOPMAP_DUPLICATE, _last_offset, 0, map, _last);
    } else {
      /* not empty, not an identical copy of the previous entry */
      size = size_for(map);
      _mapping[i].set(Mapping::OOPMAP_NEW, _offset, size, map);
      _last_offset = _offset;
      _last = map;
    }

    _offset += size;
  }

  int total = base + pairs + _offset;
  _required = total;
  return total;
}

void ImmutableOopMapBuilder::fill_pair(ImmutableOopMapPair* pair, const OopMap* map, int offset, const ImmutableOopMapSet* set) {
  new ((address) pair) ImmutableOopMapPair(map->offset(), offset);
}

int ImmutableOopMapBuilder::fill_map(ImmutableOopMapPair* pair, const OopMap* map, int offset, const ImmutableOopMapSet* set) {
  fill_pair(pair, map, offset, set);
  address addr = (address) pair->get_from(_new_set); // location of the ImmutableOopMap

  new (addr) ImmutableOopMap(map);
  return size_for(map);
}

void ImmutableOopMapBuilder::fill(ImmutableOopMapSet* set, int sz) {
  ImmutableOopMapPair* pairs = set->get_pairs();

  for (int i = 0; i < set->count(); ++i) {
    const OopMap* map = _mapping[i]._map;
    ImmutableOopMapPair* pair = NULL;
    int size = 0;

    if (_mapping[i]._kind == Mapping::OOPMAP_NEW) {
      size = fill_map(&pairs[i], map, _mapping[i]._offset, set);
    } else if (_mapping[i]._kind == Mapping::OOPMAP_DUPLICATE || _mapping[i]._kind == Mapping::OOPMAP_EMPTY) {
      fill_pair(&pairs[i], map, _mapping[i]._offset, set);
    }

    const ImmutableOopMap* nv = set->find_map_at_offset(map->offset());
  }
}

ImmutableOopMapSet* ImmutableOopMapBuilder::generate_into(address buffer) {
  _new_set = new (buffer) ImmutableOopMapSet(_set, _required);
  fill(_new_set, _required);

  return _new_set;
}

ImmutableOopMapSet* ImmutableOopMapBuilder::build() {
  _required = heap_size();

  // We need to allocate a chunk big enough to hold the ImmutableOopMapSet and all of its ImmutableOopMaps
  address buffer = (address) NEW_C_HEAP_ARRAY(unsigned char, _required, mtCode);
  return generate_into(buffer);
}

ImmutableOopMapSet* ImmutableOopMapSet::build_from(const OopMapSet* oopmap_set) {
  ResourceMark mark;
  ImmutableOopMapBuilder builder(oopmap_set);
  return builder.build();
}

//------------------------------DerivedPointerTable---------------------------

class DerivedPointerEntry : public CHeapObj<mtCompiler> {
 private:
  oop*     _location; // Location of derived pointer (also pointing to the base)
  intptr_t _offset;   // Offset from base pointer
 public:
  DerivedPointerEntry(oop* location, intptr_t offset) { _location = location; _offset = offset; }
  oop* location()    { return _location; }
  intptr_t  offset() { return _offset; }
};

GrowableArray<DerivedPointerEntry*>* DerivedPointerTable::_list = NULL;
bool DerivedPointerTable::_active = false;

void DerivedPointerTable::clear() {
  // The first time, we create the list.  Otherwise it should be
  // empty.  If not, then we have probably forgotton to call
  // update_pointers after last GC/Scavenge.
  if (_list == NULL) {
    _list = new (ResourceObj::C_HEAP, mtCompiler) GrowableArray<DerivedPointerEntry*>(10, true); // Allocated on C heap
  }
  _active = true;
}

// Returns value of location as an int
intptr_t value_of_loc(oop *pointer) { return cast_from_oop<intptr_t>((*pointer)); }

void DerivedPointerTable::add(oop *derived_loc, oop *base_loc) {
  if (_active) {
    intptr_t offset = value_of_loc(derived_loc) - value_of_loc(base_loc);
    // Set derived oop location to point to base.
    *derived_loc = (oop)base_loc;
    DerivedPointerEntry *entry = new DerivedPointerEntry(derived_loc, offset);
    _list->append(entry);
  }
}

void DerivedPointerTable::update_pointers() {
  for (int i = 0; i < _list->length(); i++) {
    DerivedPointerEntry* entry = _list->at(i);
    oop* derived_loc = entry->location();
    intptr_t offset  = entry->offset();
    // The derived oop was setup to point to location of base
    oop  base        = **(oop**)derived_loc;

    *derived_loc = (oop)(((address)base) + offset);

    // Delete entry
    delete entry;
    _list->at_put(i, NULL);
  }
  // Clear list, so it is ready for next traversal (this is an invariant)
  _list->clear();
  _active = false;
}
