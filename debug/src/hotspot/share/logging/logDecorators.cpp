#include "precompiled.hpp"

#include "logging/logDecorators.hpp"
#include "runtime/os.inline.hpp"

const LogDecorators LogDecorators::None = LogDecorators(0);

const char* LogDecorators::_name[][2] = {
#define DECORATOR(n, a) {#n, #a},
  DECORATOR_LIST
#undef DECORATOR
};

LogDecorators::Decorator LogDecorators::from_string(const char* str) {
  for (size_t i = 0; i < Count; i++) {
    Decorator d = static_cast<Decorator>(i);
    if (strcasecmp(str, name(d)) == 0 || strcasecmp(str, abbreviation(d)) == 0) {
      return d;
    }
  }
  return Invalid;
}

bool LogDecorators::parse(const char* decorator_args, outputStream* errstream) {
  if (decorator_args == NULL || strlen(decorator_args) == 0) {
    _decorators = DefaultDecoratorsMask;
    return true;
  }

  if (strcasecmp(decorator_args, "none") == 0 ) {
    _decorators = 0;
    return true;
  }

  bool result = true;
  uint tmp_decorators = 0;
  char* args_copy = os::strdup_check_oom(decorator_args, mtLogging);
  char* token = args_copy;
  char* comma_pos;
  do {
    comma_pos = strchr(token, ',');
    if (comma_pos != NULL) {
      *comma_pos = '\0';
    }
    Decorator d = from_string(token);
    if (d == Invalid) {
      if (errstream != NULL) {
        errstream->print_cr("Invalid decorator '%s'.", token);
      }
      result = false;
      break;
    }
    tmp_decorators |= mask(d);
    token = comma_pos + 1;
  } while (comma_pos != NULL);
  os::free(args_copy);
  if (result) {
    _decorators = tmp_decorators;
  }
  return result;
}
