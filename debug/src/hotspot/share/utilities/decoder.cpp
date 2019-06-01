#include "precompiled.hpp"

#include "jvm.h"
#include "memory/allocation.inline.hpp"
#include "runtime/os.hpp"
#include "utilities/decoder.hpp"
#include "utilities/vmError.hpp"

#if defined(__APPLE__)
  #include "decoder_machO.hpp"
#else
  #include "decoder_elf.hpp"
#endif

AbstractDecoder*  Decoder::_shared_decoder = NULL;
AbstractDecoder*  Decoder::_error_handler_decoder = NULL;
NullDecoder       Decoder::_do_nothing_decoder;
Mutex*            Decoder::_shared_decoder_lock = new Mutex(Mutex::native, "SharedDecoderLock", false, Monitor::_safepoint_check_never);

AbstractDecoder* Decoder::get_shared_instance() {

  if (_shared_decoder == NULL) {
    _shared_decoder = create_decoder();
  }
  return _shared_decoder;
}

AbstractDecoder* Decoder::get_error_handler_instance() {
  if (_error_handler_decoder == NULL) {
    _error_handler_decoder = create_decoder();
  }
  return _error_handler_decoder;
}

AbstractDecoder* Decoder::create_decoder() {
  AbstractDecoder* decoder;
#if defined (__APPLE__)
  decoder = new (std::nothrow)MachODecoder();
#else
  decoder = new (std::nothrow)ElfDecoder();
#endif

  if (decoder == NULL || decoder->has_error()) {
    if (decoder != NULL) {
      delete decoder;
    }
    decoder = &_do_nothing_decoder;
  }
  return decoder;
}

inline bool DecoderLocker::is_first_error_thread() {
  return (os::current_thread_id() == VMError::get_first_error_tid());
}

DecoderLocker::DecoderLocker() :
  MutexLockerEx(DecoderLocker::is_first_error_thread() ?
                NULL : Decoder::shared_decoder_lock(), true) {
  _decoder = is_first_error_thread() ?
    Decoder::get_error_handler_instance() : Decoder::get_shared_instance();
}

Mutex* Decoder::shared_decoder_lock() {
  return _shared_decoder_lock;
}

bool Decoder::decode(address addr, char* buf, int buflen, int* offset, const char* modulepath, bool demangle) {
  bool error_handling_thread = os::current_thread_id() == VMError::first_error_tid;
  MutexLockerEx locker(error_handling_thread ? NULL : _shared_decoder_lock, true);
  AbstractDecoder* decoder = error_handling_thread ?
    get_error_handler_instance(): get_shared_instance();

  return decoder->decode(addr, buf, buflen, offset, modulepath, demangle);
}

bool Decoder::decode(address addr, char* buf, int buflen, int* offset, const void* base) {
  bool error_handling_thread = os::current_thread_id() == VMError::first_error_tid;
  MutexLockerEx locker(error_handling_thread ? NULL : _shared_decoder_lock, true);
  AbstractDecoder* decoder = error_handling_thread ?
    get_error_handler_instance(): get_shared_instance();

  return decoder->decode(addr, buf, buflen, offset, base);
}

bool Decoder::demangle(const char* symbol, char* buf, int buflen) {
  bool error_handling_thread = os::current_thread_id() == VMError::first_error_tid;
  MutexLockerEx locker(error_handling_thread ? NULL : _shared_decoder_lock, true);
  AbstractDecoder* decoder = error_handling_thread ?
    get_error_handler_instance(): get_shared_instance();
  return decoder->demangle(symbol, buf, buflen);
}

void Decoder::print_state_on(outputStream* st) { }

bool Decoder::get_source_info(address pc, char* buf, size_t buflen, int* line) {
  return false;
}
