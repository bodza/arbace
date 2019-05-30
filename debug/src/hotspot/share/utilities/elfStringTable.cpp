#include "precompiled.hpp"

#if !defined(__APPLE__)

#include "jvm.h"
#include "memory/allocation.inline.hpp"
#include "runtime/os.hpp"
#include "utilities/elfStringTable.hpp"

// We will try to load whole string table into memory if we can.
// Otherwise, fallback to more expensive file operation.
ElfStringTable::ElfStringTable(FILE* const file, Elf_Shdr& shdr, int index) :
  _section(file, shdr), _index(index), _fd(file), _next(NULL) {
  _status = _section.status();
}

ElfStringTable::~ElfStringTable() {
  if (_next != NULL) {
    delete _next;
  }
}

bool ElfStringTable::string_at(size_t pos, char* buf, int buflen) {
  if (NullDecoder::is_error(get_status())) {
    return false;
  }

  assert(buflen > 0, "no buffer");
  if (pos >= _section.section_header()->sh_size) {
    return false;
  }

  const char* data = (const char*)_section.section_data();
  if (data != NULL) {
    jio_snprintf(buf, buflen, "%s", data + pos);
    return true;
  } else {  // no cache data, read from file instead
    const Elf_Shdr* const shdr = _section.section_header();
    MarkedFileReader mfd(_fd);
    if (mfd.has_mark() &&
      mfd.set_position(shdr->sh_offset + pos) &&
      mfd.read((void*)buf, size_t(buflen))) {
      buf[buflen - 1] = '\0';
      return true;
    } else {
      // put it in error state to avoid retry
      _status = NullDecoder::file_invalid;
      return false;
    }
  }
}

#endif
