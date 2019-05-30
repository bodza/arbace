#ifndef SHARE_VM_UTILITIES_JSON_HPP
#define SHARE_VM_UTILITIES_JSON_HPP

#include "memory/allocation.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/ostream.hpp"

class JSON : public ResourceObj {
 protected:
  JSON(const char* text, bool silent, outputStream* st);
  void parse();
  bool valid();

  typedef enum {
    JSON_NONE,
    JSON_OBJECT_BEGIN,
    JSON_OBJECT_END,
    JSON_ARRAY_BEGIN,
    JSON_ARRAY_END,
    JSON_KEY,
    JSON_STRING,
    JSON_NUMBER_INT,
    JSON_NUMBER_FLOAT,
    JSON_TRUE,
    JSON_FALSE,
    JSON_NULL
  } JSON_TYPE;

  typedef union {
    int64_t int_value;
    uint64_t uint_value;
    double double_value;

    struct {
      const char* start;
      size_t length;
    } str;
  } JSON_VAL;

  typedef enum {
    INTERNAL_ERROR,
    SYNTAX_ERROR,
    KEY_ERROR,
    VALUE_ERROR
  } JSON_ERROR;

  void error(JSON_ERROR e, const char* format, ...) ATTRIBUTE_PRINTF(3, 4);
  outputStream* _st;

 private:
  const char* start;
  const char* pos;

  // For error printing
  const char* mark; // Error marker
  uint level;
  uint line;
  uint column;

  bool silent;
  bool _valid;

  bool parse_json_value();
  bool parse_json_object();
  bool parse_json_array();
  bool parse_json_string(bool key = false);
  bool parse_json_key();
  bool parse_json_number();
  bool parse_json_symbol(const char* name, JSON_TYPE symbol);

  virtual bool callback(JSON_TYPE t, JSON_VAL* v, uint level) = 0;

  void mark_pos();
  u_char next();
  u_char peek();
  u_char peek(size_t i);
  int expect_any(const char* valid_chars, const char* error_msg, JSON_ERROR e = SYNTAX_ERROR);
  bool expect_string(const char* expected_string, const char* error_msg = "", JSON_ERROR e = SYNTAX_ERROR);
  size_t skip(size_t i);
  int skip_to_token();
  u_char skip_to(u_char want);
  u_char skip_line_comment();
  int skip_block_comment();

  const char* strerror(JSON_ERROR e);
};

#endif
