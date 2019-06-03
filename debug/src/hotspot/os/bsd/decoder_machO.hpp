#ifndef OS_BSD_VM_DECODER_MACHO_HPP
#define OS_BSD_VM_DECODER_MACHO_HPP

#ifdef __APPLE__

#include "utilities/decoder.hpp"

// Just a placehold for now, a real implementation should derive
// from AbstractDecoder
class MachODecoder : public AbstractDecoder {
 public:
  MachODecoder() { }
  virtual ~MachODecoder() { }
  virtual bool demangle(const char* symbol, char* buf, int buflen);
  virtual bool decode(address pc, char* buf, int buflen, int* offset, const void* base);
  virtual bool decode(address pc, char* buf, int buflen, int* offset, const char* module_path, bool demangle) {
    ShouldNotReachHere();
    return false;
  }

 private:
  void * mach_find_command(struct mach_header_64 * mach_base, uint32_t command_wanted);
  char * mach_find_in_stringtable(char *strtab, uint32_t tablesize, int strx_wanted);
};

#endif

#endif
