#include "precompiled.hpp"
#include "code/codeCache.hpp"
#include "code/compiledIC.hpp"
#include "code/icBuffer.hpp"
#include "code/nmethod.hpp"
#include "code/scopeDesc.hpp"
#include "gc/shared/collectedHeap.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/mutexLocker.hpp"
#include "runtime/stubRoutines.hpp"

DEF_STUB_INTERFACE(ICStub);

StubQueue* InlineCacheBuffer::_buffer    = NULL;
ICStub*    InlineCacheBuffer::_next_stub = NULL;

CompiledICHolder* InlineCacheBuffer::_pending_released = NULL;
int InlineCacheBuffer::_pending_count = 0;

void ICStub::finalize() {
  if (!is_empty()) {
    ResourceMark rm;
    CompiledIC *ic = CompiledIC_at(CodeCache::find_compiled(ic_site()), ic_site());

    ic->set_ic_destination_and_value(destination(), cached_value());
  }
}

address ICStub::destination() const {
  return InlineCacheBuffer::ic_buffer_entry_point(code_begin());
}

void* ICStub::cached_value() const {
  return InlineCacheBuffer::ic_buffer_cached_value(code_begin());
}

void ICStub::set_stub(CompiledIC *ic, void* cached_val, address dest_addr) {
  // We cannot store a pointer to the 'ic' object, since it is resource allocated. Instead we
  // store the location of the inline cache. Then we have enough information recreate the CompiledIC
  // object when we need to remove the stub.
  _ic_site = ic->instruction_address();

  // Assemble new stub
  InlineCacheBuffer::assemble_ic_buffer_code(code_begin(), cached_val, dest_addr);
}

void ICStub::clear() {
  if (CompiledIC::is_icholder_entry(destination())) {
    InlineCacheBuffer::queue_for_release((CompiledICHolder*)cached_value());
  }
  _ic_site = NULL;
}

//-----------------------------------------------------------------------------------------------
// Implementation of InlineCacheBuffer

void InlineCacheBuffer::init_next_stub() {
  ICStub* ic_stub = (ICStub*)buffer()->request_committed (ic_stub_code_size());
  set_next_stub(ic_stub);
}

void InlineCacheBuffer::initialize() {
  if (_buffer != NULL) return; // already initialized
  _buffer = new StubQueue(new ICStubInterface, 10*K, InlineCacheBuffer_lock, "InlineCacheBuffer");
  init_next_stub();
}

ICStub* InlineCacheBuffer::new_ic_stub() {
  while (true) {
    ICStub* ic_stub = (ICStub*)buffer()->request_committed(ic_stub_code_size());
    if (ic_stub != NULL) {
      return ic_stub;
    }
    // we ran out of inline cache buffer space; must enter safepoint.
    // We do this by forcing a safepoint
    EXCEPTION_MARK;

    VM_ICBufferFull ibf;
    VMThread::execute(&ibf);
    // We could potential get an async. exception at this point.
    // In that case we will rethrow it to ourselvs.
    if (HAS_PENDING_EXCEPTION) {
      oop exception = PENDING_EXCEPTION;
      CLEAR_PENDING_EXCEPTION;
      Thread::send_async_exception(JavaThread::current()->threadObj(), exception);
    }
  }
  ShouldNotReachHere();
  return NULL;
}

void InlineCacheBuffer::update_inline_caches() {
  if (buffer()->number_of_stubs() > 1) {
    if (TraceICBuffer) {
      tty->print_cr("[updating inline caches with %d stubs]", buffer()->number_of_stubs());
    }
    buffer()->remove_all();
    init_next_stub();
  }
  release_pending_icholders();
}

bool InlineCacheBuffer::contains(address instruction_address) {
  return buffer()->contains(instruction_address);
}

bool InlineCacheBuffer::is_empty() {
  return buffer()->number_of_stubs() == 1;    // always has sentinel
}

void InlineCacheBuffer_init() {
  InlineCacheBuffer::initialize();
}

void InlineCacheBuffer::create_transition_stub(CompiledIC *ic, void* cached_value, address entry) {
  if (TraceICBuffer) {
    tty->print_cr("  create transition stub for " INTPTR_FORMAT " destination " INTPTR_FORMAT " cached value " INTPTR_FORMAT,
                  p2i(ic->instruction_address()), p2i(entry), p2i(cached_value));
  }

  // If an transition stub is already associate with the inline cache, then we remove the association.
  if (ic->is_in_transition_state()) {
    ICStub* old_stub = ICStub_from_destination_address(ic->stub_address());
    old_stub->clear();
  }

  // allocate and initialize new "out-of-line" inline-cache
  ICStub* ic_stub = get_next_stub();
  ic_stub->set_stub(ic, cached_value, entry);

  // Update inline cache in nmethod to point to new "out-of-line" allocated inline cache
  ic->set_ic_destination(ic_stub);

  set_next_stub(new_ic_stub()); // can cause safepoint synchronization
}

address InlineCacheBuffer::ic_destination_for(CompiledIC *ic) {
  ICStub* stub = ICStub_from_destination_address(ic->stub_address());
  return stub->destination();
}

void* InlineCacheBuffer::cached_value_for(CompiledIC *ic) {
  ICStub* stub = ICStub_from_destination_address(ic->stub_address());
  return stub->cached_value();
}

// Free CompiledICHolder*s that are no longer in use
void InlineCacheBuffer::release_pending_icholders() {
  CompiledICHolder* holder = _pending_released;
  _pending_released = NULL;
  while (holder != NULL) {
    CompiledICHolder* next = holder->next();
    delete holder;
    holder = next;
    _pending_count--;
  }
}

// Enqueue this icholder for release during the next safepoint.  It's
// not safe to free them until them since they might be visible to
// another thread.
void InlineCacheBuffer::queue_for_release(CompiledICHolder* icholder) {
  MutexLockerEx mex(InlineCacheBuffer_lock);
  icholder->set_next(_pending_released);
  _pending_released = icholder;
  _pending_count++;
  if (TraceICBuffer) {
    tty->print_cr("enqueueing icholder " INTPTR_FORMAT " to be freed", p2i(icholder));
  }
}
