#ifndef SHARE_VM_RUNTIME_SAFEPOINTMECHANISM_HPP
#define SHARE_VM_RUNTIME_SAFEPOINTMECHANISM_HPP

#include "runtime/globals.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"
#include "utilities/sizes.hpp"

// This is the abstracted interface for the safepoint implementation
class SafepointMechanism : public AllStatic {
  enum PollingType {
    _global_page_poll,
    _thread_local_poll
  };
  static PollingType _polling_type;
  static void* _poll_armed_value;
  static void* _poll_disarmed_value;
  static void set_uses_thread_local_poll()            { _polling_type     = _thread_local_poll; }

  static void* poll_armed_value()                     { return _poll_armed_value; }
  static void* poll_disarmed_value()                  { return _poll_disarmed_value; }

  static inline bool local_poll_armed(JavaThread* thread);

  static inline bool local_poll(Thread* thread);
  static inline bool global_poll();

  static inline void block_if_requested_local_poll(JavaThread *thread);

  static void default_initialize();
  static void initialize_serialize_page();

  static void pd_initialize() { default_initialize(); };

  // By adding 8 to the base address of the protected polling page we can differentiate
  // between the armed and disarmed value by masking out this bit.
  const static intptr_t _poll_bit = 8;
public:
  static intptr_t poll_bit() { return _poll_bit; }

  static bool uses_global_page_poll() { return _polling_type == _global_page_poll; }
  static bool uses_thread_local_poll() { return _polling_type == _thread_local_poll; }

  static bool supports_thread_local_poll() {
#ifdef THREAD_LOCAL_POLL
    return true;
#else
    return false;
#endif
  }

  // Call this method to see if this thread has depending poll and appropriate action should be taken
  static inline bool poll(Thread* thread);

  // Blocks a thread until safepoint is completed
  static inline void block_if_requested(JavaThread* thread);

  // Caller is responsible for using a memory barrier if needed.
  static inline void arm_local_poll(JavaThread* thread);
  static inline void disarm_local_poll(JavaThread* thread);

  static inline void arm_local_poll_release(JavaThread* thread);
  static inline void disarm_local_poll_release(JavaThread* thread);

  // Setup the selected safepoint mechanism
  static void initialize();
  static void initialize_header(JavaThread* thread);
};

#endif
