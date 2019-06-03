#include "precompiled.hpp"

#if !defined(__APPLE__)

#include "memory/allocation.inline.hpp"
#include "utilities/elfFuncDescTable.hpp"

ElfFuncDescTable::ElfFuncDescTable(FILE* file, Elf_Shdr shdr, int index) :
  _file(file), _index(index), _section(file, shdr) {
  _status = _section.status();
}

ElfFuncDescTable::~ElfFuncDescTable() { }

address ElfFuncDescTable::lookup(Elf_Word index) {
  if (NullDecoder::is_error(_status)) {
    return NULL;
  }

  address*  func_descs = cached_func_descs();
  const Elf_Shdr* shdr = _section.section_header();
  if (!(shdr->sh_size > 0 && shdr->sh_addr <= index && index <= shdr->sh_addr + shdr->sh_size)) {
    // don't put the whole decoder in error mode if we just tried a wrong index
    return NULL;
  }

  if (func_descs != NULL) {
    return func_descs[(index - shdr->sh_addr) / sizeof(address)];
  } else {
    MarkedFileReader mfd(_file);
    address addr;
    if (!mfd.has_mark() || !mfd.set_position(shdr->sh_offset + index - shdr->sh_addr) || !mfd.read((void*)&addr, sizeof(addr))) {
      _status = NullDecoder::file_invalid;
      return NULL;
    }
    return addr;
  }
}

#endif
