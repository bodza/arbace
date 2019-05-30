#include "precompiled.hpp"

#if !defined(__APPLE__)
#include "decoder_elf.hpp"
#include "memory/allocation.inline.hpp"

ElfDecoder::~ElfDecoder() {
  if (_opened_elf_files != NULL) {
    delete _opened_elf_files;
    _opened_elf_files = NULL;
  }
}

bool ElfDecoder::decode(address addr, char *buf, int buflen, int* offset, const char* filepath, bool demangle_name) {
  assert(filepath, "null file path");
  assert(buf != NULL && buflen > 0, "Invalid buffer");
  if (has_error()) return false;
  ElfFile* file = get_elf_file(filepath);
  if (file == NULL) {
    return false;
  }

  if (!file->decode(addr, buf, buflen, offset)) {
    return false;
  }
  if (demangle_name && (buf[0] != '\0')) {
    demangle(buf, buf, buflen);
  }
  return true;
}

ElfFile* ElfDecoder::get_elf_file(const char* filepath) {
  ElfFile* file;

  file = _opened_elf_files;
  while (file != NULL) {
    if (file->same_elf_file(filepath)) {
      return file;
    }
    file = file->next();
  }

  file = new (std::nothrow)ElfFile(filepath);
  if (file != NULL) {
    if (_opened_elf_files != NULL) {
      file->set_next(_opened_elf_files);
    }
    _opened_elf_files = file;
  }

  return file;
}
#endif
