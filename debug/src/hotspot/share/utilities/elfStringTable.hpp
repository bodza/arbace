#ifndef SHARE_VM_UTILITIES_ELF_STRING_TABLE_HPP
#define SHARE_VM_UTILITIES_ELF_STRING_TABLE_HPP

#if !defined(__APPLE__)

#include "memory/allocation.hpp"
#include "utilities/decoder.hpp"
#include "utilities/elfFile.hpp"

// The string table represents a string table section in an elf file.
// Whenever there is enough memory, it will load whole string table as
// one blob. Otherwise, it will load string from file when requested.
class ElfStringTable: CHeapObj<mtInternal> {
  friend class ElfFile;
private:
  ElfStringTable*   _next;
  int               _index;     // section index
  ElfSection        _section;
  FILE* const       _fd;
  NullDecoder::decoder_status _status;

public:
  ElfStringTable(FILE* const file, Elf_Shdr& shdr, int index);
  ~ElfStringTable();

  // section index
  int index() const { return _index; };

  // get string at specified offset
  bool string_at(size_t offset, char* buf, int buflen);

  // get status code
  NullDecoder::decoder_status get_status() const {
    return _status;
  }

private:
  void set_next(ElfStringTable* next) {
    _next = next;
  }

  ElfStringTable* next() const {
    return _next;
  }
};

#endif

#endif
