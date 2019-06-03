#include "precompiled.hpp"

#include "utilities/debug.hpp"
#include "utilities/stringUtils.hpp"

int StringUtils::replace_no_expand(char* string, const char* from, const char* to) {
  int replace_count = 0;
  size_t from_len = strlen(from);
  size_t to_len = strlen(to);

  for (char* dst = string; *dst && (dst = strstr(dst, from)) != NULL;) {
    char* left_over = dst + from_len;
    memmove(dst, to, to_len);                       // does not copy trailing 0 of <to>
    dst += to_len;                                  // skip over the replacement.
    memmove(dst, left_over, strlen(left_over) + 1); // copies the trailing 0 of <left_over>
    ++ replace_count;
  }

  return replace_count;
}

double StringUtils::similarity(const char* str1, size_t len1, const char* str2, size_t len2) {
  // filter out zero-length strings else we will underflow on len-1 below
  if (len1 == 0 || len2 == 0) {
    return 0.0;
  }

  size_t total = len1 + len2;
  size_t hit = 0;

  for (size_t i = 0; i < len1 - 1; i++) {
    for (size_t j = 0; j < len2 - 1; j++) {
      if ((str1[i] == str2[j]) && (str1[i+1] == str2[j+1])) {
        ++hit;
        break;
      }
    }
  }

  return 2.0 * (double) hit / (double) total;
}
