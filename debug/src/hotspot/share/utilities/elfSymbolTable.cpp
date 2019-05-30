#include "precompiled.hpp"

#if !defined(__APPLE__)

#include "memory/allocation.inline.hpp"
#include "utilities/elfFuncDescTable.hpp"
#include "utilities/elfSymbolTable.hpp"

ElfSymbolTable::ElfSymbolTable(FILE* const file, Elf_Shdr& shdr) :
  _section(file, shdr), _fd(file), _next(NULL) {
  assert(file != NULL, "null file handle");
  _status = _section.status();

  if (_section.section_header()->sh_size % sizeof(Elf_Sym) != 0) {
    _status = NullDecoder::file_invalid;
  }
}

ElfSymbolTable::~ElfSymbolTable() {
  if (_next != NULL) {
    delete _next;
  }
}

bool ElfSymbolTable::compare(const Elf_Sym* sym, address addr, int* stringtableIndex, int* posIndex, int* offset, ElfFuncDescTable* funcDescTable) {
  if (STT_FUNC == ELF_ST_TYPE(sym->st_info)) {
    Elf_Word st_size = sym->st_size;
    const Elf_Shdr* shdr = _section.section_header();
    address sym_addr;
    if (funcDescTable != NULL && funcDescTable->get_index() == sym->st_shndx) {
      // We need to go another step trough the function descriptor table (currently PPC64 only)
      sym_addr = funcDescTable->lookup(sym->st_value);
    } else {
      sym_addr = (address)sym->st_value;
    }
    if (sym_addr <= addr && (Elf_Word)(addr - sym_addr) < st_size) {
      *offset = (int)(addr - sym_addr);
      *posIndex = sym->st_name;
      *stringtableIndex = shdr->sh_link;
      return true;
    }
  }
  return false;
}

bool ElfSymbolTable::lookup(address addr, int* stringtableIndex, int* posIndex, int* offset, ElfFuncDescTable* funcDescTable) {
  assert(stringtableIndex, "null string table index pointer");
  assert(posIndex, "null string table offset pointer");
  assert(offset, "null offset pointer");

  if (NullDecoder::is_error(get_status())) {
    return false;
  }

  size_t  sym_size = sizeof(Elf_Sym);
  int count = _section.section_header()->sh_size / sym_size;
  Elf_Sym* symbols = (Elf_Sym*)_section.section_data();

  if (symbols != NULL) {
    for (int index = 0; index < count; index ++) {
      if (compare(&symbols[index], addr, stringtableIndex, posIndex, offset, funcDescTable)) {
        return true;
      }
    }
  } else {
    MarkedFileReader mfd(_fd);

    if (!mfd.has_mark() || !mfd.set_position(_section.section_header()->sh_offset)) {
      _status = NullDecoder::file_invalid;
      return false;
    }

    Elf_Sym sym;
    for (int index = 0; index < count; index ++) {
      if (!mfd.read((void*)&sym, sizeof(sym))) {
        _status = NullDecoder::file_invalid;
        return false;
      }

      if (compare(&sym, addr, stringtableIndex, posIndex, offset, funcDescTable)) {
        return true;
      }
    }
  }
  return false;
}

#endif
