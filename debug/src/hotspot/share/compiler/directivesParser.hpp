#ifndef SHARE_VM_COMPILER_DIRECTIVESPARSER_HPP
#define SHARE_VM_COMPILER_DIRECTIVESPARSER_HPP

#include "utilities/json.hpp"
#include "compiler/compilerDirectives.hpp"

enum FlagType {
  boolFlag,
  intxFlag,
  uintxFlag,
  doubleFlag,
  ccstrFlag,
  ccstrlistFlag,
  UnknownFlagType
};

static const char* flag_type_names[] = {
    "bool",
    "int",
    "uint",
    "double",
    "string",
    "string list",
    "unknown"
};

class DirectivesParser : public JSON {
public:
  static bool has_file();
  static bool parse_from_flag();
  static bool parse_from_file(const char* filename, outputStream* st);
  static int  parse_string(const char* string, outputStream* st);
  int install_directives();

private:
  DirectivesParser(const char* text, outputStream* st, bool silent);
  ~DirectivesParser();

  bool callback(JSON_TYPE t, JSON_VAL* v, uint level);
  static bool parse_from_file_inner(const char* filename, outputStream* st);

  // types of "keys". i.e recognized <key>:<value> pairs in our JSON syntax
  typedef enum {
     type_c1,
     type_c2,
     type_enable,
     type_preset,
     type_match,
     type_inline,

     // After here, there is no correlation between
     // keytype and keys array
     //type_strategy,
     type_flag,
     //type_dir,

     // Synthetic.
     type_dir_array,
     type_directives,
     type_value_array
  } keytype;

  // name, type, dtd info and maybe a setter
  // this is how we map key-values
  typedef struct {
     const char *name;
     keytype     type;
     uint    allow_array_value : 1;
     uint    allowedmask;
     void (DirectiveSet::*set)(void* arg);
     FlagType flag_type;
  } key;

  // Array with valid keys for the directive file
  static const key keys[];
  // Marker for outermost moosewings/array
  static const key dir_array_key;
  // Marker for a directives set (these are "implicit" objects, as in not named)
  static const key dir_key;
  // Marker for a multi value
  static const key value_array_key;

  // A compiler directive shouldn't be able to use more than 5 stack slots.
  // Example of max stack usage:
  // depth 1: type_dir_array  [
  // depth 2: type_directives   {
  // depth 3: type_c1             c1: {
  // depth 4: type_inline           inline:
  // depth 5: type_value_array      [ ...
  static const uint MAX_DEPTH = 5;
  const key* stack[MAX_DEPTH];
  uint depth;

  bool push_key(const char* str, size_t len);
  bool push_key(const key* k);
  const key* current_key();
  const key* pop_key();
  static const key* lookup_key(const char* s, size_t len);

  bool set_option(JSON_TYPE t, JSON_VAL* v);
  bool set_option_flag(JSON_TYPE t, JSON_VAL* v, const key* option_key, DirectiveSet* set);

  CompilerDirectives* current_directive;
  DirectiveSet*       current_directiveset;

  void push_tmp(CompilerDirectives* dir);
  void clean_tmp();
  CompilerDirectives* pop_tmp();
  CompilerDirectives* _tmp_top; // temporary storage for dirs while parsing
  int _tmp_depth;               // Number of directives that has been parsed but not installed.

  static uint mask(keytype kt);
};

#endif
