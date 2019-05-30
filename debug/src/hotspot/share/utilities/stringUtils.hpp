#ifndef SHARE_VM_UTILITIES_STRINGUTILS_HPP
#define SHARE_VM_UTILITIES_STRINGUTILS_HPP

#include "memory/allocation.hpp"

class StringUtils : AllStatic {
public:
  // Replace the substring <from> with another string <to>. <to> must be
  // no longer than <from>. The input string is modified in-place.
  //
  // Replacement is done in a single pass left-to-right. So replace_no_expand("aaa", "aa", "a")
  // will result in "aa", not "a".
  //
  // Returns the count of substrings that have been replaced.
  static int replace_no_expand(char* string, const char* from, const char* to);

  // Compute string similarity based on Dice's coefficient
  static double similarity(const char* str1, size_t len1, const char* str2, size_t len2);
};

#endif
