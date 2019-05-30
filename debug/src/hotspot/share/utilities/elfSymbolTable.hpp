#ifndef SHARE_VM_UTILITIES_ELF_SYMBOL_TABLE_HPP
#define SHARE_VM_UTILITIES_ELF_SYMBOL_TABLE_HPP

#if !defined(__APPLE__)

#include "memory/allocation.hpp"
#include "utilities/decoder.hpp"
#include "utilities/elfFile.hpp"

/*
 * symbol table object represents a symbol section in an elf file.
 * Whenever possible, it will load all symbols from the corresponding section
 * of the elf file into memory. Otherwise, it will walk the section in file
 * to look up the symbol that nearest the given address.
 */
class ElfSymbolTable: public CHeapObj<mtInternal> {
  friend class ElfFile;
private:
  ElfSymbolTable*  _next;

  // file contains string table
  FILE* const      _fd;

  // corresponding section
  ElfSection      _section;

  NullDecoder::decoder_status _status;
public:
  ElfSymbolTable(FILE* const file, Elf_Shdr& shdr);
  ~ElfSymbolTable();

  // search the symbol that is nearest to the specified address.
  bool lookup(address addr, int* stringtableIndex, int* posIndex, int* offset, ElfFuncDescTable* funcDescTable);

  NullDecoder::decoder_status get_status() const { return _status; };
private:
  ElfSymbolTable* next() const { return _next; }
  void set_next(ElfSymbolTable* next) { _next = next; }

  bool compare(const Elf_Sym* sym, address addr, int* stringtableIndex, int* posIndex, int* offset, ElfFuncDescTable* funcDescTable);
};

#endif

#endif
