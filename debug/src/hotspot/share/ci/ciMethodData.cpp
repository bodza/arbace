#include "precompiled.hpp"

#include "ci/ciMetadata.hpp"
#include "ci/ciMethodData.hpp"
#include "ci/ciUtilities.inline.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/resourceArea.hpp"
#include "utilities/copy.hpp"

// ciMethodData

ciMethodData::ciMethodData(MethodData* md) : ciMetadata(md) {
  Copy::zero_to_words((HeapWord*) &_orig, sizeof(_orig) / sizeof(HeapWord));
  _data = NULL;
  _data_size = 0;
  _extra_data_size = 0;
  _current_mileage = 0;
  _invocation_counter = 0;
  _backedge_counter = 0;
  _state = empty_state;
  _saw_free_extra_data = false;
  // Set an initial hint. Don't use set_hint_di() because
  // first_di() may be out of bounds if data_size is 0.
  _hint_di = first_di();
  // Initialize the escape information (to "don't know.");
  _eflags = _arg_local = _arg_stack = _arg_returned = 0;
  _parameters = NULL;
}

// No MethodData*.
ciMethodData::ciMethodData() : ciMetadata(NULL) {
  Copy::zero_to_words((HeapWord*) &_orig, sizeof(_orig) / sizeof(HeapWord));
  _data = NULL;
  _data_size = 0;
  _extra_data_size = 0;
  _current_mileage = 0;
  _invocation_counter = 0;
  _backedge_counter = 0;
  _state = empty_state;
  _saw_free_extra_data = false;
  // Set an initial hint. Don't use set_hint_di() because
  // first_di() may be out of bounds if data_size is 0.
  _hint_di = first_di();
  // Initialize the escape information (to "don't know.");
  _eflags = _arg_local = _arg_stack = _arg_returned = 0;
  _parameters = NULL;
}

void ciMethodData::load_extra_data() {
  MethodData* mdo = get_MethodData();

  MutexLocker ml(mdo->extra_data_lock());

  // speculative trap entries also hold a pointer to a Method so need to be translated
  DataLayout* dp_src  = mdo->extra_data_base();
  DataLayout* end_src = mdo->args_data_limit();
  DataLayout* dp_dst  = extra_data_base();
  for (;; dp_src = MethodData::next_extra(dp_src), dp_dst = MethodData::next_extra(dp_dst)) {
    // New traps in the MDO may have been added since we copied the
    // data (concurrent deoptimizations before we acquired
    // extra_data_lock above) or can be removed (a safepoint may occur
    // in the translate_from call below) as we translate the copy:
    // update the copy as we go.
    int tag = dp_src->tag();
    if (tag != DataLayout::arg_info_data_tag) {
      memcpy(dp_dst, dp_src, ((intptr_t)MethodData::next_extra(dp_src)) - ((intptr_t)dp_src));
    }

    switch (tag) {
    case DataLayout::speculative_trap_data_tag: {
      ciSpeculativeTrapData data_dst(dp_dst);
      SpeculativeTrapData   data_src(dp_src);

      { // During translation a safepoint can happen or VM lock can be taken (e.g., Compile_lock).
        MutexUnlocker ml(mdo->extra_data_lock());
        data_dst.translate_from(&data_src);
      }
      break;
    }
    case DataLayout::bit_data_tag:
      break;
    case DataLayout::no_tag:
    case DataLayout::arg_info_data_tag:
      // An empty slot or ArgInfoData entry marks the end of the trap data
      {
        return; // Need a block to avoid SS compiler bug
      }
    default:
      fatal("bad tag = %d", tag);
    }
  }
}

void ciMethodData::load_data() {
  MethodData* mdo = get_MethodData();
  if (mdo == NULL) {
    return;
  }

  // To do: don't copy the data if it is not "ripe" -- require a minimum #
  // of invocations.

  // Snapshot the data -- actually, take an approximate snapshot of
  // the data.  Any concurrently executing threads may be changing the
  // data as we copy it.
  Copy::disjoint_words((HeapWord*) mdo,
                       (HeapWord*) &_orig,
                       sizeof(_orig) / HeapWordSize);
  Arena* arena = ciEnv::current()->arena();
  _data_size = mdo->data_size();
  _extra_data_size = mdo->extra_data_size();
  int total_size = _data_size + _extra_data_size;
  _data = (intptr_t *) arena->Amalloc(total_size);
  Copy::disjoint_words((HeapWord*) mdo->data_base(), (HeapWord*) _data, total_size / HeapWordSize);

  // Traverse the profile data, translating any oops into their
  // ci equivalents.
  ResourceMark rm;
  ciProfileData* ci_data = first_data();
  ProfileData* data = mdo->first_data();
  while (is_valid(ci_data)) {
    ci_data->translate_from(data);
    ci_data = next_data(ci_data);
    data = mdo->next_data(data);
  }
  if (mdo->parameters_type_data() != NULL) {
    _parameters = data_layout_at(mdo->parameters_type_data_di());
    ciParametersTypeData* parameters = new ciParametersTypeData(_parameters);
    parameters->translate_from(mdo->parameters_type_data());
  }

  load_extra_data();

  // Note:  Extra data are all BitData, and do not need translation.
  _current_mileage = MethodData::mileage_of(mdo->method());
  _invocation_counter = mdo->invocation_count();
  _backedge_counter = mdo->backedge_count();
  _state = mdo->is_mature() ? mature_state : immature_state;

  _eflags = mdo->eflags();
  _arg_local = mdo->arg_local();
  _arg_stack = mdo->arg_stack();
  _arg_returned  = mdo->arg_returned();
}

void ciReceiverTypeData::translate_receiver_data_from(const ProfileData* data) {
  for (uint row = 0; row < row_limit(); row++) {
    Klass* k = data->as_ReceiverTypeData()->receiver(row);
    if (k != NULL) {
      ciKlass* klass = ciEnv::current()->get_klass(k);
      set_receiver(row, klass);
    }
  }
}

void ciTypeStackSlotEntries::translate_type_data_from(const TypeStackSlotEntries* entries) {
  for (int i = 0; i < number_of_entries(); i++) {
    intptr_t k = entries->type(i);
    TypeStackSlotEntries::set_type(i, translate_klass(k));
  }
}

void ciReturnTypeEntry::translate_type_data_from(const ReturnTypeEntry* ret) {
  intptr_t k = ret->type();
  set_type(translate_klass(k));
}

void ciSpeculativeTrapData::translate_from(const ProfileData* data) {
  Method* m = data->as_SpeculativeTrapData()->method();
  ciMethod* ci_m = ciEnv::current()->get_method(m);
  set_method(ci_m);
}

// Get the data at an arbitrary (sort of) data index.
ciProfileData* ciMethodData::data_at(int data_index) {
  if (out_of_bounds(data_index)) {
    return NULL;
  }
  DataLayout* data_layout = data_layout_at(data_index);

  switch (data_layout->tag()) {
  case DataLayout::no_tag:
  default:
    ShouldNotReachHere();
    return NULL;
  case DataLayout::bit_data_tag:
    return new ciBitData(data_layout);
  case DataLayout::counter_data_tag:
    return new ciCounterData(data_layout);
  case DataLayout::jump_data_tag:
    return new ciJumpData(data_layout);
  case DataLayout::receiver_type_data_tag:
    return new ciReceiverTypeData(data_layout);
  case DataLayout::virtual_call_data_tag:
    return new ciVirtualCallData(data_layout);
  case DataLayout::ret_data_tag:
    return new ciRetData(data_layout);
  case DataLayout::branch_data_tag:
    return new ciBranchData(data_layout);
  case DataLayout::multi_branch_data_tag:
    return new ciMultiBranchData(data_layout);
  case DataLayout::arg_info_data_tag:
    return new ciArgInfoData(data_layout);
  case DataLayout::call_type_data_tag:
    return new ciCallTypeData(data_layout);
  case DataLayout::virtual_call_type_data_tag:
    return new ciVirtualCallTypeData(data_layout);
  case DataLayout::parameters_type_data_tag:
    return new ciParametersTypeData(data_layout);
  };
}

// Iteration over data.
ciProfileData* ciMethodData::next_data(ciProfileData* current) {
  int current_index = dp_to_di(current->dp());
  int next_index = current_index + current->size_in_bytes();
  ciProfileData* next = data_at(next_index);
  return next;
}

ciProfileData* ciMethodData::bci_to_extra_data(int bci, ciMethod* m, bool& two_free_slots) {
  DataLayout* dp  = extra_data_base();
  DataLayout* end = args_data_limit();
  two_free_slots = false;
  for (;dp < end; dp = MethodData::next_extra(dp)) {
    switch (dp->tag()) {
    case DataLayout::no_tag:
      _saw_free_extra_data = true;  // observed an empty slot (common case)
      two_free_slots = (MethodData::next_extra(dp)->tag() == DataLayout::no_tag);
      return NULL;
    case DataLayout::arg_info_data_tag:
      return NULL; // ArgInfoData is at the end of extra data section.
    case DataLayout::bit_data_tag:
      if (m == NULL && dp->bci() == bci) {
        return new ciBitData(dp);
      }
      break;
    case DataLayout::speculative_trap_data_tag: {
      ciSpeculativeTrapData* data = new ciSpeculativeTrapData(dp);
      // data->method() might be null if the MDO is snapshotted
      // concurrently with a trap
      if (m != NULL && data->method() == m && dp->bci() == bci) {
        return data;
      }
      break;
    }
    default:
      fatal("bad tag = %d", dp->tag());
    }
  }
  return NULL;
}

// Translate a bci to its corresponding data, or NULL.
ciProfileData* ciMethodData::bci_to_data(int bci, ciMethod* m) {
  // If m is not NULL we look for a SpeculativeTrapData entry
  if (m == NULL) {
    ciProfileData* data = data_before(bci);
    for ( ; is_valid(data); data = next_data(data)) {
      if (data->bci() == bci) {
        set_hint_di(dp_to_di(data->dp()));
        return data;
      } else if (data->bci() > bci) {
        break;
      }
    }
  }
  bool two_free_slots = false;
  ciProfileData* result = bci_to_extra_data(bci, m, two_free_slots);
  if (result != NULL) {
    return result;
  }
  if (m != NULL && !two_free_slots) {
    // We were looking for a SpeculativeTrapData entry we didn't
    // find. Room is not available for more SpeculativeTrapData
    // entries, look in the non SpeculativeTrapData entries.
    return bci_to_data(bci, NULL);
  }
  return NULL;
}

void ciMethodData::clear_escape_info() {
  VM_ENTRY_MARK;
  MethodData* mdo = get_MethodData();
  if (mdo != NULL) {
    mdo->clear_escape_info();
    ArgInfoData *aid = arg_info();
    int arg_count = (aid == NULL) ? 0 : aid->number_of_args();
    for (int i = 0; i < arg_count; i++) {
      set_arg_modified(i, 0);
    }
  }
  _eflags = _arg_local = _arg_stack = _arg_returned = 0;
}

// copy our escape info to the MethodData* if it exists
void ciMethodData::update_escape_info() {
  VM_ENTRY_MARK;
  MethodData* mdo = get_MethodData();
  if (mdo != NULL) {
    mdo->set_eflags(_eflags);
    mdo->set_arg_local(_arg_local);
    mdo->set_arg_stack(_arg_stack);
    mdo->set_arg_returned(_arg_returned);
    int arg_count = mdo->method()->size_of_parameters();
    for (int i = 0; i < arg_count; i++) {
      mdo->set_arg_modified(i, arg_modified(i));
    }
  }
}

void ciMethodData::set_compilation_stats(short loops, short blocks) {
  VM_ENTRY_MARK;
  MethodData* mdo = get_MethodData();
  if (mdo != NULL) {
    mdo->set_num_loops(loops);
    mdo->set_num_blocks(blocks);
  }
}

void ciMethodData::set_would_profile(bool p) {
  VM_ENTRY_MARK;
  MethodData* mdo = get_MethodData();
  if (mdo != NULL) {
    mdo->set_would_profile(p);
  }
}

void ciMethodData::set_argument_type(int bci, int i, ciKlass* k) {
  VM_ENTRY_MARK;
  MethodData* mdo = get_MethodData();
  if (mdo != NULL) {
    ProfileData* data = mdo->bci_to_data(bci);
    if (data != NULL) {
      if (data->is_CallTypeData()) {
        data->as_CallTypeData()->set_argument_type(i, k->get_Klass());
      } else {
        data->as_VirtualCallTypeData()->set_argument_type(i, k->get_Klass());
      }
    }
  }
}

void ciMethodData::set_parameter_type(int i, ciKlass* k) {
  VM_ENTRY_MARK;
  MethodData* mdo = get_MethodData();
  if (mdo != NULL) {
    mdo->parameters_type_data()->set_type(i, k->get_Klass());
  }
}

void ciMethodData::set_return_type(int bci, ciKlass* k) {
  VM_ENTRY_MARK;
  MethodData* mdo = get_MethodData();
  if (mdo != NULL) {
    ProfileData* data = mdo->bci_to_data(bci);
    if (data != NULL) {
      if (data->is_CallTypeData()) {
        data->as_CallTypeData()->set_return_type(k->get_Klass());
      } else {
        data->as_VirtualCallTypeData()->set_return_type(k->get_Klass());
      }
    }
  }
}

bool ciMethodData::has_escape_info() {
  return eflag_set(MethodData::estimated);
}

void ciMethodData::set_eflag(MethodData::EscapeFlag f) {
  set_bits(_eflags, f);
}

void ciMethodData::clear_eflag(MethodData::EscapeFlag f) {
  clear_bits(_eflags, f);
}

bool ciMethodData::eflag_set(MethodData::EscapeFlag f) const {
  return mask_bits(_eflags, f) != 0;
}

void ciMethodData::set_arg_local(int i) {
  set_nth_bit(_arg_local, i);
}

void ciMethodData::set_arg_stack(int i) {
  set_nth_bit(_arg_stack, i);
}

void ciMethodData::set_arg_returned(int i) {
  set_nth_bit(_arg_returned, i);
}

void ciMethodData::set_arg_modified(int arg, uint val) {
  ArgInfoData *aid = arg_info();
  if (aid == NULL)
    return;
  aid->set_arg_modified(arg, val);
}

bool ciMethodData::is_arg_local(int i) const {
  return is_set_nth_bit(_arg_local, i);
}

bool ciMethodData::is_arg_stack(int i) const {
  return is_set_nth_bit(_arg_stack, i);
}

bool ciMethodData::is_arg_returned(int i) const {
  return is_set_nth_bit(_arg_returned, i);
}

uint ciMethodData::arg_modified(int arg) const {
  ArgInfoData *aid = arg_info();
  if (aid == NULL)
    return 0;
  return aid->arg_modified(arg);
}

ByteSize ciMethodData::offset_of_slot(ciProfileData* data, ByteSize slot_offset_in_data) {
  // Get offset within MethodData* of the data array
  ByteSize data_offset = MethodData::data_offset();

  // Get cell offset of the ProfileData within data array
  int cell_offset = dp_to_di(data->dp());

  // Add in counter_offset, the # of bytes into the ProfileData of counter or flag
  int offset = in_bytes(data_offset) + cell_offset + in_bytes(slot_offset_in_data);

  return in_ByteSize(offset);
}

ciArgInfoData *ciMethodData::arg_info() const {
  // Should be last, have to skip all traps.
  DataLayout* dp  = extra_data_base();
  DataLayout* end = args_data_limit();
  for ( ; dp < end; dp = MethodData::next_extra(dp)) {
    if (dp->tag() == DataLayout::arg_info_data_tag)
      return new ciArgInfoData(dp);
  }
  return NULL;
}

// Implementation of the print method.
void ciMethodData::print_impl(outputStream* st) {
  ciMetadata::print_impl(st);
}

void ciMethodData::dump_replay_data_type_helper(outputStream* out, int round, int& count, ProfileData* pdata, ByteSize offset, ciKlass* k) {
  if (k != NULL) {
    if (round == 0) {
      count++;
    } else {
      out->print(" %d %s", (int)(dp_to_di(pdata->dp() + in_bytes(offset)) / sizeof(intptr_t)), k->name()->as_quoted_ascii());
    }
  }
}

template<class T> void ciMethodData::dump_replay_data_receiver_type_helper(outputStream* out, int round, int& count, T* vdata) {
  for (uint i = 0; i < vdata->row_limit(); i++) {
    dump_replay_data_type_helper(out, round, count, vdata, vdata->receiver_offset(i), vdata->receiver(i));
  }
}

template<class T> void ciMethodData::dump_replay_data_call_type_helper(outputStream* out, int round, int& count, T* call_type_data) {
  if (call_type_data->has_arguments()) {
    for (int i = 0; i < call_type_data->number_of_arguments(); i++) {
      dump_replay_data_type_helper(out, round, count, call_type_data, call_type_data->argument_type_offset(i), call_type_data->valid_argument_type(i));
    }
  }
  if (call_type_data->has_return()) {
    dump_replay_data_type_helper(out, round, count, call_type_data, call_type_data->return_type_offset(), call_type_data->valid_return_type());
  }
}

void ciMethodData::dump_replay_data_extra_data_helper(outputStream* out, int round, int& count) {
  DataLayout* dp  = extra_data_base();
  DataLayout* end = args_data_limit();

  for (;dp < end; dp = MethodData::next_extra(dp)) {
    switch (dp->tag()) {
    case DataLayout::no_tag:
    case DataLayout::arg_info_data_tag:
      return;
    case DataLayout::bit_data_tag:
      break;
    case DataLayout::speculative_trap_data_tag: {
      ciSpeculativeTrapData* data = new ciSpeculativeTrapData(dp);
      ciMethod* m = data->method();
      if (m != NULL) {
        if (round == 0) {
          count++;
        } else {
          out->print(" %d ", (int)(dp_to_di(((address)dp) + in_bytes(ciSpeculativeTrapData::method_offset())) / sizeof(intptr_t)));
          m->dump_name_as_ascii(out);
        }
      }
      break;
    }
    default:
      fatal("bad tag = %d", dp->tag());
    }
  }
}
