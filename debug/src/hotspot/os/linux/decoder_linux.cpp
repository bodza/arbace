#include "jvm.h"
#include "utilities/decoder_elf.hpp"
#include "utilities/elfFile.hpp"

#include <cxxabi.h>

bool ElfDecoder::demangle(const char* symbol, char *buf, int buflen) {
  int   status;
  char* result;
  size_t size = (size_t)buflen;

  // Don't pass buf to __cxa_demangle. In case of the 'buf' is too small,
  // __cxa_demangle will call system "realloc" for additional memory, which
  // may use different malloc/realloc mechanism that allocates 'buf'.
  if ((result = abi::__cxa_demangle(symbol, NULL, NULL, &status)) != NULL) {
    jio_snprintf(buf, buflen, "%s", result);
      // call c library's free
      ::free(result);
      return true;
  }
  return false;
}

// Returns true if the elf file is marked NOT to require an executable stack,
// or if the file could not be opened.
// Returns false if the elf file requires an executable stack, the stack flag
// is not set at all, or if the file can not be read.
bool ElfFile::specifies_noexecstack(const char* filepath) {
  if (filepath == NULL) return true;

  FILE* file = fopen(filepath, "r");
  if (file == NULL)  return true;

  // AARCH64 defaults to noexecstack. All others default to execstack.
  bool result = AARCH64_ONLY(true) NOT_AARCH64(false);

  // Read file header
  Elf_Ehdr head;
  if (fread(&head, sizeof(Elf_Ehdr), 1, file) == 1 && is_elf_file(head) && fseek(file, head.e_phoff, SEEK_SET) == 0) {
    // Read program header table
    Elf_Phdr phdr;
    for (int index = 0; index < head.e_phnum; index ++) {
      if (fread((void*)&phdr, sizeof(Elf_Phdr), 1, file) != 1) {
        result = false;
        break;
      }
      if (phdr.p_type == PT_GNU_STACK) {
        result = (phdr.p_flags == (PF_R | PF_W));
        break;
      }
    }
  }
  fclose(file);
  return result;
}
