#include "precompiled.hpp"

#include "logging/log.hpp"
#include "runtime/globals.hpp"
#include "runtime/os.hpp"
#include "runtime/safepointMechanism.inline.hpp"
#include "services/memTracker.hpp"
#include "utilities/globalDefinitions.hpp"

SafepointMechanism::PollingType SafepointMechanism::_polling_type = SafepointMechanism::_global_page_poll;
void* SafepointMechanism::_poll_armed_value;
void* SafepointMechanism::_poll_disarmed_value;

void SafepointMechanism::default_initialize() {
  if (ThreadLocalHandshakes) {
    set_uses_thread_local_poll();

    // Poll bit values
    intptr_t poll_armed_value = poll_bit();
    intptr_t poll_disarmed_value = 0;

#ifdef USE_POLL_BIT_ONLY
    if (!USE_POLL_BIT_ONLY)
#endif
    {
      // Polling page
      const size_t page_size = os::vm_page_size();
      const size_t allocation_size = 2 * page_size;
      char* polling_page = os::reserve_memory(allocation_size, NULL, page_size);
      os::commit_memory_or_exit(polling_page, allocation_size, false, "Unable to commit Safepoint polling page");
      MemTracker::record_virtual_memory_type((address)polling_page, mtInternal);

      char* bad_page  = polling_page;
      char* good_page = polling_page + page_size;

      os::protect_memory(bad_page,  page_size, os::MEM_PROT_NONE);
      os::protect_memory(good_page, page_size, os::MEM_PROT_READ);

      os::set_polling_page((address)(bad_page));

      // Poll address values
      intptr_t bad_page_val  = reinterpret_cast<intptr_t>(bad_page),
               good_page_val = reinterpret_cast<intptr_t>(good_page);
      poll_armed_value    |= bad_page_val;
      poll_disarmed_value |= good_page_val;
    }

    _poll_armed_value    = reinterpret_cast<void*>(poll_armed_value);
    _poll_disarmed_value = reinterpret_cast<void*>(poll_disarmed_value);
  } else {
    const size_t page_size = os::vm_page_size();
    char* polling_page = os::reserve_memory(page_size, NULL, page_size);
    os::commit_memory_or_exit(polling_page, page_size, false, "Unable to commit Safepoint polling page");
    os::protect_memory(polling_page, page_size, os::MEM_PROT_READ);

    os::set_polling_page((address)(polling_page));
  }
}

void SafepointMechanism::initialize_header(JavaThread* thread) {
  disarm_local_poll(thread);
}

void SafepointMechanism::initialize_serialize_page() {
  if (!UseMembar) {
    const size_t page_size = os::vm_page_size();
    char* serialize_page = os::reserve_memory(page_size, NULL, page_size);
    os::commit_memory_or_exit(serialize_page, page_size, false, "Unable to commit memory serialization page");
    os::set_memory_serialize_page((address)(serialize_page));
  }
}

void SafepointMechanism::initialize() {
  pd_initialize();
  initialize_serialize_page();
}
