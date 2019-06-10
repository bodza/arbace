#include "precompiled.hpp"

#include "classfile/altHashing.hpp"
#include "classfile/classLoaderData.inline.hpp"
#include "classfile/javaClasses.inline.hpp"
#include "classfile/moduleEntry.hpp"
#include "classfile/stringTable.hpp"
#include "classfile/vmSymbols.hpp"
#include "code/debugInfo.hpp"
#include "code/pcDesc.hpp"
#include "interpreter/linkResolver.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "oops/fieldStreams.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/instanceMirrorKlass.hpp"
#include "oops/klass.hpp"
#include "oops/method.inline.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/symbol.hpp"
#include "oops/typeArrayOop.inline.hpp"
#include "runtime/fieldDescriptor.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/interfaceSupport.inline.hpp"
#include "runtime/java.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/jniHandles.inline.hpp"
#include "runtime/safepoint.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "runtime/thread.inline.hpp"
#include "runtime/vframe.inline.hpp"
#include "utilities/align.hpp"
#include "utilities/preserveException.hpp"
#include "jvmci/jvmciJavaClasses.hpp"

#define INJECTED_FIELD_COMPUTE_OFFSET(klass, name, signature, may_be_java) \
  klass::_##name##_offset = JavaClasses::compute_injected_offset(JavaClasses::klass##_##name##_enum);

#define DECLARE_INJECTED_FIELD(klass, name, signature, may_be_java) \
  { SystemDictionary::WK_KLASS_ENUM_NAME(klass), vmSymbols::VM_SYMBOL_ENUM_NAME(name##_name), vmSymbols::VM_SYMBOL_ENUM_NAME(signature), may_be_java },

InjectedField JavaClasses::_injected_fields[] = {
  ALL_INJECTED_FIELDS(DECLARE_INJECTED_FIELD)
};

int JavaClasses::compute_injected_offset(InjectedFieldID id) {
  return _injected_fields[id].compute_offset();
}

InjectedField* JavaClasses::get_injected(Symbol* class_name, int* field_count) {
  *field_count = 0;

  vmSymbols::SID sid = vmSymbols::find_sid(class_name);
  if (sid == vmSymbols::NO_SID) {
    // Only well known classes can inject fields
    return NULL;
  }

  int count = 0;
  int start = -1;

#define LOOKUP_INJECTED_FIELD(klass, name, signature, may_be_java) \
  if (sid == vmSymbols::VM_SYMBOL_ENUM_NAME(klass)) { \
    count++; \
    if (start == -1) start = klass##_##name##_enum; \
  }
  ALL_INJECTED_FIELDS(LOOKUP_INJECTED_FIELD);
#undef LOOKUP_INJECTED_FIELD

  if (start != -1) {
    *field_count = count;
    return _injected_fields + start;
  }
  return NULL;
}

// Helpful routine for computing field offsets at run time rather than hardcoding them
// Finds local fields only, including static fields.  Static field offsets are from the
// beginning of the mirror.
static void compute_offset(int &dest_offset, InstanceKlass* ik, Symbol* name_symbol, Symbol* signature_symbol, bool is_static = false) {
  fieldDescriptor fd;
  if (ik == NULL) {
    ResourceMark rm;
    vm_exit_during_initialization("Invalid layout of preloaded class");
  }

  if (!ik->find_local_field(name_symbol, signature_symbol, &fd) || fd.is_static() != is_static) {
    ResourceMark rm;
    vm_exit_during_initialization("Invalid layout of preloaded class: use -Xlog:class+load=info to see the origin of the problem class");
  }
  dest_offset = fd.offset();
}

// Overloading to pass name as a string.
static void compute_offset(int& dest_offset, InstanceKlass* ik, const char* name_string, Symbol* signature_symbol, bool is_static = false) {
  TempNewSymbol name = SymbolTable::probe(name_string, (int)strlen(name_string));
  if (name == NULL) {
    ResourceMark rm;
    vm_exit_during_initialization("Invalid layout of preloaded class", ik->external_name());
  }
  compute_offset(dest_offset, ik, name, signature_symbol, is_static);
}

// Same as above but for "optional" offsets that might not be present in certain JDK versions
// Old versions should be cleaned out since Hotspot only supports the current JDK, and this
// function should be removed.
static void
compute_optional_offset(int& dest_offset, InstanceKlass* ik, Symbol* name_symbol, Symbol* signature_symbol) {
  fieldDescriptor fd;
  if (ik->find_local_field(name_symbol, signature_symbol, &fd)) {
    dest_offset = fd.offset();
  }
}

int java_lang_String::value_offset  = 0;
int java_lang_String::hash_offset   = 0;
int java_lang_String::coder_offset  = 0;

bool java_lang_String::initialized  = false;

bool java_lang_String::is_instance(oop obj) {
  return is_instance_inlined(obj);
}

#define FIELD_COMPUTE_OFFSET(offset, klass, name, signature, is_static) \
  compute_offset(offset, klass, name, vmSymbols::signature(), is_static)

#define FIELD_COMPUTE_OFFSET_OPTIONAL(offset, klass, name, signature) \
  compute_optional_offset(offset, klass, name, vmSymbols::signature())

#define STRING_FIELDS_DO(macro) \
  macro(value_offset, k, vmSymbols::value_name(), byte_array_signature, false); \
  macro(hash_offset,  k, "hash",                  int_signature,        false); \
  macro(coder_offset, k, "coder",                 byte_signature,       false)

void java_lang_String::compute_offsets() {
  if (initialized) {
    return;
  }

  InstanceKlass* k = SystemDictionary::String_klass();
  STRING_FIELDS_DO(FIELD_COMPUTE_OFFSET);

  initialized = true;
}

class CompactStringsFixup : public FieldClosure {
private:
  bool _value;

public:
  CompactStringsFixup(bool value) : _value(value) { }

  void do_field(fieldDescriptor* fd) {
    if (fd->name() == vmSymbols::compact_strings_name()) {
      oop mirror = fd->field_holder()->java_mirror();
      mirror->bool_field_put(fd->offset(), _value);
    }
  }
};

void java_lang_String::set_compact_strings(bool value) {
  CompactStringsFixup fix(value);
  InstanceKlass::cast(SystemDictionary::String_klass())->do_local_static_fields(&fix);
}

Handle java_lang_String::basic_create(int length, bool is_latin1, TRAPS) {
  // Create the String object first, so there's a chance that the String
  // and the char array it points to end up in the same cache line.
  oop obj;
  obj = SystemDictionary::String_klass()->allocate_instance(CHECK_NH);

  // Create the char array.  The String object must be handlized here
  // because GC can happen as a result of the allocation attempt.
  Handle h_obj(THREAD, obj);
  int arr_length = is_latin1 ? length : length << 1; // 2 bytes per UTF16.
  typeArrayOop buffer = oopFactory::new_byteArray(arr_length, CHECK_NH);

  // Point the String at the char array
  obj = h_obj();
  set_value(obj, buffer);
  // No need to zero the offset, allocation zero'ed the entire String object
  set_coder(obj, is_latin1 ? CODER_LATIN1 : CODER_UTF16);
  return h_obj;
}

Handle java_lang_String::create_from_unicode(jchar* unicode, int length, TRAPS) {
  bool is_latin1 = CompactStrings && UNICODE::is_latin1(unicode, length);
  Handle h_obj = basic_create(length, is_latin1, CHECK_NH);
  typeArrayOop buffer = value(h_obj());
  if (is_latin1) {
    for (int index = 0; index < length; index++) {
      buffer->byte_at_put(index, (jbyte)unicode[index]);
    }
  } else {
    for (int index = 0; index < length; index++) {
      buffer->char_at_put(index, unicode[index]);
    }
  }

  return h_obj;
}

oop java_lang_String::create_oop_from_unicode(jchar* unicode, int length, TRAPS) {
  Handle h_obj = create_from_unicode(unicode, length, CHECK_0);
  return h_obj();
}

Handle java_lang_String::create_from_str(const char* utf8_str, TRAPS) {
  if (utf8_str == NULL) {
    return Handle();
  }
  bool has_multibyte, is_latin1;
  int length = UTF8::unicode_length(utf8_str, is_latin1, has_multibyte);
  if (!CompactStrings) {
    has_multibyte = true;
    is_latin1 = false;
  }

  Handle h_obj = basic_create(length, is_latin1, CHECK_NH);
  if (length > 0) {
    if (!has_multibyte) {
      const jbyte* src = reinterpret_cast<const jbyte*>(utf8_str);
      ArrayAccess<>::arraycopy_from_native(src, value(h_obj()), typeArrayOopDesc::element_offset<jbyte>(0), length);
    } else if (is_latin1) {
      UTF8::convert_to_unicode(utf8_str, value(h_obj())->byte_at_addr(0), length);
    } else {
      UTF8::convert_to_unicode(utf8_str, value(h_obj())->char_at_addr(0), length);
    }
  }

  return h_obj;
}

oop java_lang_String::create_oop_from_str(const char* utf8_str, TRAPS) {
  Handle h_obj = create_from_str(utf8_str, CHECK_0);
  return h_obj();
}

Handle java_lang_String::create_from_symbol(Symbol* symbol, TRAPS) {
  const char* utf8_str = (char*)symbol->bytes();
  int utf8_len = symbol->utf8_length();

  bool has_multibyte, is_latin1;
  int length = UTF8::unicode_length(utf8_str, utf8_len, is_latin1, has_multibyte);
  if (!CompactStrings) {
    has_multibyte = true;
    is_latin1 = false;
  }

  Handle h_obj = basic_create(length, is_latin1, CHECK_NH);
  if (length > 0) {
    if (!has_multibyte) {
      const jbyte* src = reinterpret_cast<const jbyte*>(utf8_str);
      ArrayAccess<>::arraycopy_from_native(src, value(h_obj()), typeArrayOopDesc::element_offset<jbyte>(0), length);
    } else if (is_latin1) {
      UTF8::convert_to_unicode(utf8_str, value(h_obj())->byte_at_addr(0), length);
    } else {
      UTF8::convert_to_unicode(utf8_str, value(h_obj())->char_at_addr(0), length);
    }
  }

  return h_obj;
}

// Converts a C string to a Java String based on current encoding
Handle java_lang_String::create_from_platform_dependent_str(const char* str, TRAPS) {
  typedef jstring (*to_java_string_fn_t)(JNIEnv*, const char *);
  static to_java_string_fn_t _to_java_string_fn = NULL;

  if (_to_java_string_fn == NULL) {
    void *lib_handle = os::native_java_library();
    _to_java_string_fn = CAST_TO_FN_PTR(to_java_string_fn_t, os::dll_lookup(lib_handle, "NewStringPlatform"));
    if (_to_java_string_fn == NULL) {
      fatal("NewStringPlatform missing");
    }
  }

  jstring js = NULL;
  { JavaThread* thread = (JavaThread*)THREAD;
    HandleMark hm(thread);
    ThreadToNativeFromVM ttn(thread);
    js = (_to_java_string_fn)(thread->jni_environment(), str);
  }
  return Handle(THREAD, JNIHandles::resolve(js));
}

// Converts a Java String to a native C string that can be used for
// native OS calls.
char* java_lang_String::as_platform_dependent_str(Handle java_string, TRAPS) {
  typedef char* (*to_platform_string_fn_t)(JNIEnv*, jstring, bool*);
  static to_platform_string_fn_t _to_platform_string_fn = NULL;

  if (_to_platform_string_fn == NULL) {
    void *lib_handle = os::native_java_library();
    _to_platform_string_fn = CAST_TO_FN_PTR(to_platform_string_fn_t, os::dll_lookup(lib_handle, "GetStringPlatformChars"));
    if (_to_platform_string_fn == NULL) {
      fatal("GetStringPlatformChars missing");
    }
  }

  char *native_platform_string;
  { JavaThread* thread = (JavaThread*)THREAD;
    JNIEnv *env = thread->jni_environment();
    jstring js = (jstring) JNIHandles::make_local(env, java_string());
    bool is_copy;
    HandleMark hm(thread);
    ThreadToNativeFromVM ttn(thread);
    native_platform_string = (_to_platform_string_fn)(env, js, &is_copy);
    JNIHandles::destroy_local(js);
  }
  return native_platform_string;
}

Handle java_lang_String::char_converter(Handle java_string, jchar from_char, jchar to_char, TRAPS) {
  oop          obj    = java_string();
  // Typical usage is to convert all '/' to '.' in string.
  typeArrayOop value  = java_lang_String::value(obj);
  int          length = java_lang_String::length(obj);
  bool      is_latin1 = java_lang_String::is_latin1(obj);

  // First check if any from_char exist
  int index; // Declared outside, used later
  for (index = 0; index < length; index++) {
    jchar c = !is_latin1 ? value->char_at(index) :
                  ((jchar) value->byte_at(index)) & 0xff;
    if (c == from_char) {
      break;
    }
  }
  if (index == length) {
    // No from_char, so do not copy.
    return java_string;
  }

  // Check if result string will be latin1
  bool to_is_latin1 = false;

  // Replacement char must be latin1
  if (CompactStrings && UNICODE::is_latin1(to_char)) {
    if (is_latin1) {
      // Source string is latin1 as well
      to_is_latin1 = true;
    } else if (!UNICODE::is_latin1(from_char)) {
      // We are replacing an UTF16 char. Scan string to
      // check if result can be latin1 encoded.
      to_is_latin1 = true;
      for (index = 0; index < length; index++) {
        jchar c = value->char_at(index);
        if (c != from_char && !UNICODE::is_latin1(c)) {
          to_is_latin1 = false;
          break;
        }
      }
    }
  }

  // Create new UNICODE (or byte) buffer. Must handlize value because GC
  // may happen during String and char array creation.
  typeArrayHandle h_value(THREAD, value);
  Handle string = basic_create(length, to_is_latin1, CHECK_NH);
  typeArrayOop from_buffer = h_value();
  typeArrayOop to_buffer = java_lang_String::value(string());

  // Copy contents
  for (index = 0; index < length; index++) {
    jchar c = (!is_latin1) ? from_buffer->char_at(index) :
                    ((jchar) from_buffer->byte_at(index)) & 0xff;
    if (c == from_char) {
      c = to_char;
    }
    if (!to_is_latin1) {
      to_buffer->char_at_put(index, c);
    } else {
      to_buffer->byte_at_put(index, (jbyte) c);
    }
  }
  return string;
}

jchar* java_lang_String::as_unicode_string(oop java_string, int& length, TRAPS) {
  typeArrayOop value  = java_lang_String::value(java_string);
               length = java_lang_String::length(java_string);
  bool      is_latin1 = java_lang_String::is_latin1(java_string);

  jchar* result = NEW_RESOURCE_ARRAY_RETURN_NULL(jchar, length);
  if (result != NULL) {
    if (!is_latin1) {
      for (int index = 0; index < length; index++) {
        result[index] = value->char_at(index);
      }
    } else {
      for (int index = 0; index < length; index++) {
        result[index] = ((jchar) value->byte_at(index)) & 0xff;
      }
    }
  } else {
    THROW_MSG_0(vmSymbols::java_lang_OutOfMemoryError(), "could not allocate Unicode string");
  }
  return result;
}

unsigned int java_lang_String::hash_code(oop java_string) {
  int          length = java_lang_String::length(java_string);
  // Zero length string will hash to zero with String.hashCode() function.
  if (length == 0) return 0;

  typeArrayOop value  = java_lang_String::value(java_string);
  bool      is_latin1 = java_lang_String::is_latin1(java_string);

  if (is_latin1) {
    return java_lang_String::hash_code(value->byte_at_addr(0), length);
  } else {
    return java_lang_String::hash_code(value->char_at_addr(0), length);
  }
}

char* java_lang_String::as_quoted_ascii(oop java_string) {
  typeArrayOop value  = java_lang_String::value(java_string);
  int          length = java_lang_String::length(java_string);
  bool      is_latin1 = java_lang_String::is_latin1(java_string);

  if (length == 0) return NULL;

  char* result;
  int result_length;
  if (!is_latin1) {
    jchar* base = value->char_at_addr(0);
    result_length = UNICODE::quoted_ascii_length(base, length) + 1;
    result = NEW_RESOURCE_ARRAY(char, result_length);
    UNICODE::as_quoted_ascii(base, length, result, result_length);
  } else {
    jbyte* base = value->byte_at_addr(0);
    result_length = UNICODE::quoted_ascii_length(base, length) + 1;
    result = NEW_RESOURCE_ARRAY(char, result_length);
    UNICODE::as_quoted_ascii(base, length, result, result_length);
  }
  return result;
}

Symbol* java_lang_String::as_symbol(oop java_string, TRAPS) {
  typeArrayOop value  = java_lang_String::value(java_string);
  int          length = java_lang_String::length(java_string);
  bool      is_latin1 = java_lang_String::is_latin1(java_string);
  if (!is_latin1) {
    jchar* base = (length == 0) ? NULL : value->char_at_addr(0);
    Symbol* sym = SymbolTable::lookup_unicode(base, length, THREAD);
    return sym;
  } else {
    ResourceMark rm;
    jbyte* position = (length == 0) ? NULL : value->byte_at_addr(0);
    const char* base = UNICODE::as_utf8(position, length);
    Symbol* sym = SymbolTable::lookup(base, length, THREAD);
    return sym;
  }
}

Symbol* java_lang_String::as_symbol_or_null(oop java_string) {
  typeArrayOop value  = java_lang_String::value(java_string);
  int          length = java_lang_String::length(java_string);
  bool      is_latin1 = java_lang_String::is_latin1(java_string);
  if (!is_latin1) {
    jchar* base = (length == 0) ? NULL : value->char_at_addr(0);
    return SymbolTable::probe_unicode(base, length);
  } else {
    ResourceMark rm;
    jbyte* position = (length == 0) ? NULL : value->byte_at_addr(0);
    const char* base = UNICODE::as_utf8(position, length);
    return SymbolTable::probe(base, length);
  }
}

int java_lang_String::utf8_length(oop java_string) {
  typeArrayOop value  = java_lang_String::value(java_string);
  int          length = java_lang_String::length(java_string);
  bool      is_latin1 = java_lang_String::is_latin1(java_string);
  if (length == 0) {
    return 0;
  }
  if (!is_latin1) {
    return UNICODE::utf8_length(value->char_at_addr(0), length);
  } else {
    return UNICODE::utf8_length(value->byte_at_addr(0), length);
  }
}

char* java_lang_String::as_utf8_string(oop java_string) {
  typeArrayOop value  = java_lang_String::value(java_string);
  int          length = java_lang_String::length(java_string);
  bool      is_latin1 = java_lang_String::is_latin1(java_string);
  if (!is_latin1) {
    jchar* position = (length == 0) ? NULL : value->char_at_addr(0);
    return UNICODE::as_utf8(position, length);
  } else {
    jbyte* position = (length == 0) ? NULL : value->byte_at_addr(0);
    return UNICODE::as_utf8(position, length);
  }
}

char* java_lang_String::as_utf8_string(oop java_string, char* buf, int buflen) {
  typeArrayOop value  = java_lang_String::value(java_string);
  int          length = java_lang_String::length(java_string);
  bool      is_latin1 = java_lang_String::is_latin1(java_string);
  if (!is_latin1) {
    jchar* position = (length == 0) ? NULL : value->char_at_addr(0);
    return UNICODE::as_utf8(position, length, buf, buflen);
  } else {
    jbyte* position = (length == 0) ? NULL : value->byte_at_addr(0);
    return UNICODE::as_utf8(position, length, buf, buflen);
  }
}

char* java_lang_String::as_utf8_string(oop java_string, int start, int len) {
  typeArrayOop value  = java_lang_String::value(java_string);
  int          length = java_lang_String::length(java_string);
  bool      is_latin1 = java_lang_String::is_latin1(java_string);
  if (!is_latin1) {
    jchar* position = value->char_at_addr(start);
    return UNICODE::as_utf8(position, len);
  } else {
    jbyte* position = value->byte_at_addr(start);
    return UNICODE::as_utf8(position, len);
  }
}

char* java_lang_String::as_utf8_string(oop java_string, int start, int len, char* buf, int buflen) {
  typeArrayOop value  = java_lang_String::value(java_string);
  int          length = java_lang_String::length(java_string);
  bool      is_latin1 = java_lang_String::is_latin1(java_string);
  if (!is_latin1) {
    jchar* position = value->char_at_addr(start);
    return UNICODE::as_utf8(position, len, buf, buflen);
  } else {
    jbyte* position = value->byte_at_addr(start);
    return UNICODE::as_utf8(position, len, buf, buflen);
  }
}

bool java_lang_String::equals(oop java_string, jchar* chars, int len) {
  typeArrayOop value = java_lang_String::value_no_keepalive(java_string);
  int length = java_lang_String::length(java_string);
  if (length != len) {
    return false;
  }
  bool is_latin1 = java_lang_String::is_latin1(java_string);
  if (!is_latin1) {
    for (int i = 0; i < len; i++) {
      if (value->char_at(i) != chars[i]) {
        return false;
      }
    }
  } else {
    for (int i = 0; i < len; i++) {
      if ((((jchar) value->byte_at(i)) & 0xff) != chars[i]) {
        return false;
      }
    }
  }
  return true;
}

bool java_lang_String::equals(oop str1, oop str2) {
  typeArrayOop value1    = java_lang_String::value_no_keepalive(str1);
  int          length1   = java_lang_String::length(str1);
  bool         is_latin1 = java_lang_String::is_latin1(str1);
  typeArrayOop value2    = java_lang_String::value_no_keepalive(str2);
  int          length2   = java_lang_String::length(str2);
  bool         is_latin2 = java_lang_String::is_latin1(str2);

  if ((length1 != length2) || (is_latin1 != is_latin2)) {
    // Strings of different size or with different
    // coders are never equal.
    return false;
  }
  int blength1 = value1->length();
  for (int i = 0; i < blength1; i++) {
    if (value1->byte_at(i) != value2->byte_at(i)) {
      return false;
    }
  }
  return true;
}

void java_lang_String::print(oop java_string, outputStream* st) {
  typeArrayOop value  = java_lang_String::value_no_keepalive(java_string);

  if (value == NULL) {
    // This can happen if, e.g., printing a String
    // object before its initializer has been called
    st->print("NULL");
    return;
  }

  int length = java_lang_String::length(java_string);
  bool is_latin1 = java_lang_String::is_latin1(java_string);

  st->print("\"");
  for (int index = 0; index < length; index++) {
    st->print("%c", (!is_latin1) ?  value->char_at(index) :
                           ((jchar) value->byte_at(index)) & 0xff );
  }
  st->print("\"");
}

static void initialize_static_field(fieldDescriptor* fd, Handle mirror, TRAPS) {
  if (fd->has_initial_value()) {
    BasicType t = fd->field_type();
    switch (t) {
      case T_BYTE:
        mirror()->byte_field_put(fd->offset(), fd->int_initial_value());
              break;
      case T_BOOLEAN:
        mirror()->bool_field_put(fd->offset(), fd->int_initial_value());
              break;
      case T_CHAR:
        mirror()->char_field_put(fd->offset(), fd->int_initial_value());
              break;
      case T_SHORT:
        mirror()->short_field_put(fd->offset(), fd->int_initial_value());
              break;
      case T_INT:
        mirror()->int_field_put(fd->offset(), fd->int_initial_value());
        break;
      case T_FLOAT:
        mirror()->float_field_put(fd->offset(), fd->float_initial_value());
        break;
      case T_DOUBLE:
        mirror()->double_field_put(fd->offset(), fd->double_initial_value());
        break;
      case T_LONG:
        mirror()->long_field_put(fd->offset(), fd->long_initial_value());
        break;
      case T_OBJECT:
        {
          oop string = fd->string_initial_value(CHECK);
          mirror()->obj_field_put(fd->offset(), string);
        }
        break;
      default:
        THROW_MSG(vmSymbols::java_lang_ClassFormatError(), "Illegal ConstantValue attribute in class file");
    }
  }
}

void java_lang_Class::fixup_mirror(Klass* k, TRAPS) {
  // If the offset was read from the shared archive, it was fixed up already
  if (!k->is_shared()) {
    if (k->is_instance_klass()) {
      // During bootstrap, java.lang.Class wasn't loaded so static field
      // offsets were computed without the size added it.  Go back and
      // update all the static field offsets to included the size.
      for (JavaFieldStream fs(InstanceKlass::cast(k)); !fs.done(); fs.next()) {
        if (fs.access_flags().is_static()) {
          int real_offset = fs.offset() + InstanceMirrorKlass::offset_of_static_fields();
          fs.set_offset(real_offset);
        }
      }
    }
  }

  if (k->is_shared() && k->has_raw_archived_mirror()) {
    k->set_java_mirror_handle(NULL);
    k->clear_has_raw_archived_mirror();
  }
  create_mirror(k, Handle(), Handle(), Handle(), CHECK);
}

void java_lang_Class::initialize_mirror_fields(Klass* k, Handle mirror, Handle protection_domain, TRAPS) {
  // Allocate a simple java object for a lock.
  // This needs to be a java object because during class initialization
  // it can be held across a java call.
  typeArrayOop r = oopFactory::new_typeArray(T_INT, 0, CHECK);
  set_init_lock(mirror(), r);

  // Set protection domain also
  set_protection_domain(mirror(), protection_domain());

  // Initialize static fields
  InstanceKlass::cast(k)->do_local_static_fields(&initialize_static_field, mirror, CHECK);
}

// Set the java.lang.Module module field in the java_lang_Class mirror
void java_lang_Class::set_mirror_module_field(Klass* k, Handle mirror, Handle module, TRAPS) {
  if (module.is_null()) {
    // During startup, the module may be NULL only if java.base has not been defined yet.
    // Put the class on the fixup_module_list to patch later when the java.lang.Module
    // for java.base is known.

    bool javabase_was_defined = false;
    {
      MutexLocker m1(Module_lock, THREAD);
      // Keep list of classes needing java.base module fixup
      if (!ModuleEntryTable::javabase_defined()) {
        k->class_loader_data()->inc_keep_alive();
        fixup_module_field_list()->push(k);
      } else {
        javabase_was_defined = true;
      }
    }

    // If java.base was already defined then patch this particular class with java.base.
    if (javabase_was_defined) {
      ModuleEntry *javabase_entry = ModuleEntryTable::javabase_moduleEntry();
      Handle javabase_handle(THREAD, javabase_entry->module());
      set_module(mirror(), javabase_handle());
    }
  } else {
    set_module(mirror(), module());
  }
}

// Statically allocate fixup lists because they always get created.
void java_lang_Class::allocate_fixup_lists() {
  GrowableArray<Klass*>* mirror_list = new (ResourceObj::C_HEAP, mtClass) GrowableArray<Klass*>(40, true);
  set_fixup_mirror_list(mirror_list);

  GrowableArray<Klass*>* module_list = new (ResourceObj::C_HEAP, mtModule) GrowableArray<Klass*>(500, true);
  set_fixup_module_field_list(module_list);
}

void java_lang_Class::create_mirror(Klass* k, Handle class_loader, Handle module, Handle protection_domain, TRAPS) {
  // Use this moment of initialization to cache modifier_flags also,
  // to support Class.getModifiers().  Instance classes recalculate
  // the cached flags after the class file is parsed, but before the
  // class is put into the system dictionary.
  int computed_modifiers = k->compute_modifier_flags(CHECK);
  k->set_modifier_flags(computed_modifiers);
  // Class_klass has to be loaded because it is used to allocate
  // the mirror.
  if (SystemDictionary::Class_klass_loaded()) {
    // Allocate mirror (java.lang.Class instance)
    oop mirror_oop = InstanceMirrorKlass::cast(SystemDictionary::Class_klass())->allocate_instance(k, CHECK);
    Handle mirror(THREAD, mirror_oop);
    Handle comp_mirror;

    // Setup indirection from mirror->klass
    java_lang_Class::set_klass(mirror(), k);

    InstanceMirrorKlass* mk = InstanceMirrorKlass::cast(mirror->klass());

    java_lang_Class::set_static_oop_field_count(mirror(), mk->compute_static_oop_field_count(mirror()));

    // It might also have a component mirror.  This mirror must already exist.
    if (k->is_array_klass()) {
      if (k->is_typeArray_klass()) {
        BasicType type = TypeArrayKlass::cast(k)->element_type();
        comp_mirror = Handle(THREAD, Universe::java_mirror(type));
      } else {
        Klass* element_klass = ObjArrayKlass::cast(k)->element_klass();
        comp_mirror = Handle(THREAD, element_klass->java_mirror());
      }

      // Two-way link between the array klass and its component mirror:
      // (array_klass) k -> mirror -> component_mirror -> array_klass -> k
      set_component_mirror(mirror(), comp_mirror());
      // See below for ordering dependencies between field array_klass in component mirror
      // and java_mirror in this klass.
    } else {
      initialize_mirror_fields(k, mirror, protection_domain, THREAD);
      if (HAS_PENDING_EXCEPTION) {
        // If any of the fields throws an exception like OOM remove the klass field
        // from the mirror so GC doesn't follow it after the klass has been deallocated.
        // This mirror looks like a primitive type, which logically it is because it
        // it represents no class.
        java_lang_Class::set_klass(mirror(), NULL);
        return;
      }
    }

    // set the classLoader field in the java_lang_Class instance
    set_class_loader(mirror(), class_loader());

    // Setup indirection from klass->mirror
    // after any exceptions can happen during allocations.
    k->set_java_mirror(mirror);

    // Set the module field in the java_lang_Class instance.  This must be done
    // after the mirror is set.
    set_mirror_module_field(k, mirror, module, THREAD);

    if (comp_mirror() != NULL) {
      // Set after k->java_mirror() is published, because compiled code running
      // concurrently doesn't expect a k to have a null java_mirror.
      release_set_array_klass(comp_mirror(), k);
    }
  } else {
    fixup_mirror_list()->push(k);
  }
}

void java_lang_Class::fixup_module_field(Klass* k, Handle module) {
  java_lang_Class::set_module(k->java_mirror(), module());
}

int java_lang_Class::oop_size(oop java_class) {
  int size = java_class->int_field(_oop_size_offset);
  return size;
}

void java_lang_Class::set_oop_size(HeapWord* java_class, int size) {
  *(int*)(((char*)java_class) + _oop_size_offset) = size;
}

int java_lang_Class::static_oop_field_count(oop java_class) {
  return java_class->int_field(_static_oop_field_count_offset);
}
void java_lang_Class::set_static_oop_field_count(oop java_class, int size) {
  java_class->int_field_put(_static_oop_field_count_offset, size);
}

oop java_lang_Class::protection_domain(oop java_class) {
  return java_class->obj_field(_protection_domain_offset);
}
void java_lang_Class::set_protection_domain(oop java_class, oop pd) {
  java_class->obj_field_put(_protection_domain_offset, pd);
}

void java_lang_Class::set_component_mirror(oop java_class, oop comp_mirror) {
    java_class->obj_field_put(_component_mirror_offset, comp_mirror);
  }
oop java_lang_Class::component_mirror(oop java_class) {
  return java_class->obj_field(_component_mirror_offset);
}

oop java_lang_Class::init_lock(oop java_class) {
  return java_class->obj_field(_init_lock_offset);
}
void java_lang_Class::set_init_lock(oop java_class, oop init_lock) {
  java_class->obj_field_put(_init_lock_offset, init_lock);
}

objArrayOop java_lang_Class::signers(oop java_class) {
  return (objArrayOop)java_class->obj_field(_signers_offset);
}
void java_lang_Class::set_signers(oop java_class, objArrayOop signers) {
  java_class->obj_field_put(_signers_offset, (oop)signers);
}

void java_lang_Class::set_class_loader(oop java_class, oop loader) {
  // jdk7 runs Queens in bootstrapping and jdk8-9 has no coordinated pushes yet.
  if (_class_loader_offset != 0) {
    java_class->obj_field_put(_class_loader_offset, loader);
  }
}

oop java_lang_Class::class_loader(oop java_class) {
  return java_class->obj_field(_class_loader_offset);
}

oop java_lang_Class::module(oop java_class) {
  return java_class->obj_field(_module_offset);
}

void java_lang_Class::set_module(oop java_class, oop module) {
  java_class->obj_field_put(_module_offset, module);
}

oop java_lang_Class::name(Handle java_class, TRAPS) {
  oop o = java_class->obj_field(_name_offset);
  if (o == NULL) {
    o = StringTable::intern(java_lang_Class::as_external_name(java_class()), THREAD);
    java_class->obj_field_put(_name_offset, o);
  }
  return o;
}

oop java_lang_Class::source_file(oop java_class) {
  return java_class->obj_field(_source_file_offset);
}

void java_lang_Class::set_source_file(oop java_class, oop source_file) {
  java_class->obj_field_put(_source_file_offset, source_file);
}

oop java_lang_Class::create_basic_type_mirror(const char* basic_type_name, BasicType type, TRAPS) {
  // This should be improved by adding a field at the Java level or by
  // introducing a new VM klass (see comment in ClassFileParser)
  oop java_class = InstanceMirrorKlass::cast(SystemDictionary::Class_klass())->allocate_instance(NULL, CHECK_0);
  if (type != T_VOID) {
    Klass* aklass = Universe::typeArrayKlassObj(type);
    release_set_array_klass(java_class, aklass);
  }
  return java_class;
}

Klass* java_lang_Class::as_Klass(oop java_class) {
  Klass* k = ((Klass*)java_class->metadata_field(_klass_offset));
  return k;
}

void java_lang_Class::set_klass(oop java_class, Klass* klass) {
  java_class->metadata_field_put(_klass_offset, klass);
}

void java_lang_Class::print_signature(oop java_class, outputStream* st) {
  Symbol* name = NULL;
  bool is_instance = false;
  if (is_primitive(java_class)) {
    name = vmSymbols::type_signature(primitive_type(java_class));
  } else {
    Klass* k = as_Klass(java_class);
    is_instance = k->is_instance_klass();
    name = k->name();
  }
  if (name == NULL) {
    st->print("<null>");
    return;
  }
  if (is_instance)  st->print("L");
  st->write((char*) name->base(), (int) name->utf8_length());
  if (is_instance)  st->print(";");
}

Symbol* java_lang_Class::as_signature(oop java_class, bool intern_if_not_found, TRAPS) {
  Symbol* name;
  if (is_primitive(java_class)) {
    name = vmSymbols::type_signature(primitive_type(java_class));
    // Because this can create a new symbol, the caller has to decrement
    // the refcount, so make adjustment here and below for symbols returned
    // that are not created or incremented due to a successful lookup.
    name->increment_refcount();
  } else {
    Klass* k = as_Klass(java_class);
    if (!k->is_instance_klass()) {
      name = k->name();
      name->increment_refcount();
    } else {
      ResourceMark rm;
      const char* sigstr = k->signature_name();
      int         siglen = (int) strlen(sigstr);
      if (!intern_if_not_found) {
        name = SymbolTable::probe(sigstr, siglen);
      } else {
        name = SymbolTable::new_symbol(sigstr, siglen, THREAD);
      }
    }
  }
  return name;
}

// Returns the Java name for this Java mirror (Resource allocated)
// See Klass::external_name().
// For primitive type Java mirrors, its type name is returned.
const char* java_lang_Class::as_external_name(oop java_class) {
  const char* name = NULL;
  if (is_primitive(java_class)) {
    name = type2name(primitive_type(java_class));
  } else {
    name = as_Klass(java_class)->external_name();
  }
  if (name == NULL) {
    name = "<null>";
  }
  return name;
}

Klass* java_lang_Class::array_klass_acquire(oop java_class) {
  Klass* k = ((Klass*)java_class->metadata_field_acquire(_array_klass_offset));
  return k;
}

void java_lang_Class::release_set_array_klass(oop java_class, Klass* klass) {
  java_class->release_metadata_field_put(_array_klass_offset, klass);
}

bool java_lang_Class::is_primitive(oop java_class) {
  bool is_primitive = (java_class->metadata_field(_klass_offset) == NULL);

  return is_primitive;
}

BasicType java_lang_Class::primitive_type(oop java_class) {
  Klass* ak = ((Klass*)java_class->metadata_field(_array_klass_offset));
  BasicType type = T_VOID;
  if (ak != NULL) {
    // Note: create_basic_type_mirror above initializes ak to a non-null value.
    type = ArrayKlass::cast(ak)->element_type();
  }
  return type;
}

BasicType java_lang_Class::as_BasicType(oop java_class, Klass** reference_klass) {
  if (is_primitive(java_class)) {
    if (reference_klass != NULL)
      (*reference_klass) = NULL;
    return primitive_type(java_class);
  } else {
    if (reference_klass != NULL)
      (*reference_klass) = as_Klass(java_class);
    return T_OBJECT;
  }
}

oop java_lang_Class::primitive_mirror(BasicType t) {
  oop mirror = Universe::java_mirror(t);
  return mirror;
}

bool java_lang_Class::offsets_computed = false;
int  java_lang_Class::classRedefinedCount_offset = -1;

#define CLASS_FIELDS_DO(macro) \
  macro(classRedefinedCount_offset, k, "classRedefinedCount", int_signature,         false) ; \
  macro(_class_loader_offset,       k, "classLoader",         classloader_signature, false); \
  macro(_component_mirror_offset,   k, "componentType",       class_signature,       false); \
  macro(_module_offset,             k, "module",              module_signature,      false); \
  macro(_name_offset,               k, "name",                string_signature,      false);

void java_lang_Class::compute_offsets() {
  if (offsets_computed) {
    return;
  }

  offsets_computed = true;

  InstanceKlass* k = SystemDictionary::Class_klass();
  CLASS_FIELDS_DO(FIELD_COMPUTE_OFFSET);

  // Init lock is a C union with component_mirror.  Only instanceKlass mirrors have
  // init_lock and only ArrayKlass mirrors have component_mirror.  Since both are oops
  // GC treats them the same.
  _init_lock_offset = _component_mirror_offset;

  CLASS_INJECTED_FIELDS(INJECTED_FIELD_COMPUTE_OFFSET);
}

int java_lang_Class::classRedefinedCount(oop the_class_mirror) {
  if (classRedefinedCount_offset == -1) {
    // If we don't have an offset for it then just return -1 as a marker.
    return -1;
  }

  return the_class_mirror->int_field(classRedefinedCount_offset);
}

void java_lang_Class::set_classRedefinedCount(oop the_class_mirror, int value) {
  if (classRedefinedCount_offset == -1) {
    // If we don't have an offset for it then nothing to set.
    return;
  }

  the_class_mirror->int_field_put(classRedefinedCount_offset, value);
}

// Note: JDK1.1 and before had a privateInfo_offset field which was used for the
//       platform thread structure, and a eetop offset which was used for thread
//       local storage (and unused by the HotSpot VM). In JDK1.2 the two structures
//       merged, so in the HotSpot VM we just use the eetop field for the thread
//       instead of the privateInfo_offset.
//
// Note: The stackSize field is only present starting in 1.4.

int java_lang_Thread::_name_offset = 0;
int java_lang_Thread::_group_offset = 0;
int java_lang_Thread::_contextClassLoader_offset = 0;
int java_lang_Thread::_inheritedAccessControlContext_offset = 0;
int java_lang_Thread::_priority_offset = 0;
int java_lang_Thread::_eetop_offset = 0;
int java_lang_Thread::_daemon_offset = 0;
int java_lang_Thread::_stillborn_offset = 0;
int java_lang_Thread::_stackSize_offset = 0;
int java_lang_Thread::_tid_offset = 0;
int java_lang_Thread::_thread_status_offset = 0;
int java_lang_Thread::_park_blocker_offset = 0;
int java_lang_Thread::_park_event_offset = 0;

#define THREAD_FIELDS_DO(macro) \
  macro(_name_offset,          k, vmSymbols::name_name(), string_signature, false); \
  macro(_group_offset,         k, vmSymbols::group_name(), threadgroup_signature, false); \
  macro(_contextClassLoader_offset, k, vmSymbols::contextClassLoader_name(), classloader_signature, false); \
  macro(_inheritedAccessControlContext_offset, k, vmSymbols::inheritedAccessControlContext_name(), accesscontrolcontext_signature, false); \
  macro(_priority_offset,      k, vmSymbols::priority_name(), int_signature, false); \
  macro(_daemon_offset,        k, vmSymbols::daemon_name(), bool_signature, false); \
  macro(_eetop_offset,         k, "eetop", long_signature, false); \
  macro(_stillborn_offset,     k, "stillborn", bool_signature, false); \
  macro(_stackSize_offset,     k, "stackSize", long_signature, false); \
  macro(_tid_offset,           k, "tid", long_signature, false); \
  macro(_thread_status_offset, k, "threadStatus", int_signature, false); \
  macro(_park_blocker_offset,  k, "parkBlocker", object_signature, false); \
  macro(_park_event_offset,    k, "nativeParkEventPointer", long_signature, false)

void java_lang_Thread::compute_offsets() {
  InstanceKlass* k = SystemDictionary::Thread_klass();
  THREAD_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

JavaThread* java_lang_Thread::thread(oop java_thread) {
  return (JavaThread*)java_thread->address_field(_eetop_offset);
}

void java_lang_Thread::set_thread(oop java_thread, JavaThread* thread) {
  java_thread->address_field_put(_eetop_offset, (address)thread);
}

oop java_lang_Thread::name(oop java_thread) {
  return java_thread->obj_field(_name_offset);
}

void java_lang_Thread::set_name(oop java_thread, oop name) {
  java_thread->obj_field_put(_name_offset, name);
}

ThreadPriority java_lang_Thread::priority(oop java_thread) {
  return (ThreadPriority)java_thread->int_field(_priority_offset);
}

void java_lang_Thread::set_priority(oop java_thread, ThreadPriority priority) {
  java_thread->int_field_put(_priority_offset, priority);
}

oop java_lang_Thread::threadGroup(oop java_thread) {
  return java_thread->obj_field(_group_offset);
}

bool java_lang_Thread::is_stillborn(oop java_thread) {
  return java_thread->bool_field(_stillborn_offset) != 0;
}

// We never have reason to turn the stillborn bit off
void java_lang_Thread::set_stillborn(oop java_thread) {
  java_thread->bool_field_put(_stillborn_offset, true);
}

bool java_lang_Thread::is_alive(oop java_thread) {
  JavaThread* thr = java_lang_Thread::thread(java_thread);
  return (thr != NULL);
}

bool java_lang_Thread::is_daemon(oop java_thread) {
  return java_thread->bool_field(_daemon_offset) != 0;
}

void java_lang_Thread::set_daemon(oop java_thread) {
  java_thread->bool_field_put(_daemon_offset, true);
}

oop java_lang_Thread::context_class_loader(oop java_thread) {
  return java_thread->obj_field(_contextClassLoader_offset);
}

oop java_lang_Thread::inherited_access_control_context(oop java_thread) {
  return java_thread->obj_field(_inheritedAccessControlContext_offset);
}

jlong java_lang_Thread::stackSize(oop java_thread) {
  if (_stackSize_offset > 0) {
    return java_thread->long_field(_stackSize_offset);
  } else {
    return 0;
  }
}

// Write the thread status value to threadStatus field in java.lang.Thread java class.
void java_lang_Thread::set_thread_status(oop java_thread, java_lang_Thread::ThreadStatus status) {
  // The threadStatus is only present starting in 1.5
  if (_thread_status_offset > 0) {
    java_thread->int_field_put(_thread_status_offset, status);
  }
}

// Read thread status value from threadStatus field in java.lang.Thread java class.
java_lang_Thread::ThreadStatus java_lang_Thread::get_thread_status(oop java_thread) {
  // Make sure the caller is operating on behalf of the VM or is
  // running VM code (state == _thread_in_vm).
  // The threadStatus is only present starting in 1.5
  if (_thread_status_offset > 0) {
    return (java_lang_Thread::ThreadStatus)java_thread->int_field(_thread_status_offset);
  } else {
    // All we can easily figure out is if it is alive, but that is
    // enough info for a valid unknown status.
    // These aren't restricted to valid set ThreadStatus values, so
    // use JVMTI values and cast.
    JavaThread* thr = java_lang_Thread::thread(java_thread);
    if (thr == NULL) {
      // the thread hasn't run yet or is in the process of exiting
      return NEW;
    }
    return (java_lang_Thread::ThreadStatus)JVMTI_THREAD_STATE_ALIVE;
  }
}

jlong java_lang_Thread::thread_id(oop java_thread) {
  // The thread ID field is only present starting in 1.5
  if (_tid_offset > 0) {
    return java_thread->long_field(_tid_offset);
  } else {
    return 0;
  }
}

oop java_lang_Thread::park_blocker(oop java_thread) {
  if (_park_blocker_offset > 0) {
    return java_thread->obj_field(_park_blocker_offset);
  }

  return NULL;
}

jlong java_lang_Thread::park_event(oop java_thread) {
  if (_park_event_offset > 0) {
    return java_thread->long_field(_park_event_offset);
  }
  return 0;
}

bool java_lang_Thread::set_park_event(oop java_thread, jlong ptr) {
  if (_park_event_offset > 0) {
    java_thread->long_field_put(_park_event_offset, ptr);
    return true;
  }
  return false;
}

const char* java_lang_Thread::thread_status_name(oop java_thread) {
  ThreadStatus status = (java_lang_Thread::ThreadStatus)java_thread->int_field(_thread_status_offset);
  switch (status) {
    case NEW                      : return "NEW";
    case RUNNABLE                 : return "RUNNABLE";
    case SLEEPING                 : return "TIMED_WAITING (sleeping)";
    case IN_OBJECT_WAIT           : return "WAITING (on object monitor)";
    case IN_OBJECT_WAIT_TIMED     : return "TIMED_WAITING (on object monitor)";
    case PARKED                   : return "WAITING (parking)";
    case PARKED_TIMED             : return "TIMED_WAITING (parking)";
    case BLOCKED_ON_MONITOR_ENTER : return "BLOCKED (on object monitor)";
    case TERMINATED               : return "TERMINATED";
    default                       : return "UNKNOWN";
  };
}
int java_lang_ThreadGroup::_parent_offset = 0;
int java_lang_ThreadGroup::_name_offset = 0;
int java_lang_ThreadGroup::_threads_offset = 0;
int java_lang_ThreadGroup::_groups_offset = 0;
int java_lang_ThreadGroup::_maxPriority_offset = 0;
int java_lang_ThreadGroup::_destroyed_offset = 0;
int java_lang_ThreadGroup::_daemon_offset = 0;
int java_lang_ThreadGroup::_nthreads_offset = 0;
int java_lang_ThreadGroup::_ngroups_offset = 0;

oop java_lang_ThreadGroup::parent(oop java_thread_group) {
  return java_thread_group->obj_field(_parent_offset);
}

// ("name as oop" accessor is not necessary)

const char* java_lang_ThreadGroup::name(oop java_thread_group) {
  oop name = java_thread_group->obj_field(_name_offset);
  // ThreadGroup.name can be null
  if (name != NULL) {
    return java_lang_String::as_utf8_string(name);
  }
  return NULL;
}

int java_lang_ThreadGroup::nthreads(oop java_thread_group) {
  return java_thread_group->int_field(_nthreads_offset);
}

objArrayOop java_lang_ThreadGroup::threads(oop java_thread_group) {
  oop threads = java_thread_group->obj_field(_threads_offset);
  return objArrayOop(threads);
}

int java_lang_ThreadGroup::ngroups(oop java_thread_group) {
  return java_thread_group->int_field(_ngroups_offset);
}

objArrayOop java_lang_ThreadGroup::groups(oop java_thread_group) {
  oop groups = java_thread_group->obj_field(_groups_offset);
  return objArrayOop(groups);
}

ThreadPriority java_lang_ThreadGroup::maxPriority(oop java_thread_group) {
  return (ThreadPriority) java_thread_group->int_field(_maxPriority_offset);
}

bool java_lang_ThreadGroup::is_destroyed(oop java_thread_group) {
  return java_thread_group->bool_field(_destroyed_offset) != 0;
}

bool java_lang_ThreadGroup::is_daemon(oop java_thread_group) {
  return java_thread_group->bool_field(_daemon_offset) != 0;
}

#define THREADGROUP_FIELDS_DO(macro) \
  macro(_parent_offset,      k, vmSymbols::parent_name(),      threadgroup_signature,       false); \
  macro(_name_offset,        k, vmSymbols::name_name(),        string_signature,            false); \
  macro(_threads_offset,     k, vmSymbols::threads_name(),     thread_array_signature,      false); \
  macro(_groups_offset,      k, vmSymbols::groups_name(),      threadgroup_array_signature, false); \
  macro(_maxPriority_offset, k, vmSymbols::maxPriority_name(), int_signature,               false); \
  macro(_destroyed_offset,   k, vmSymbols::destroyed_name(),   bool_signature,              false); \
  macro(_daemon_offset,      k, vmSymbols::daemon_name(),      bool_signature,              false); \
  macro(_nthreads_offset,    k, vmSymbols::nthreads_name(),    int_signature,               false); \
  macro(_ngroups_offset,     k, vmSymbols::ngroups_name(),     int_signature,               false)

void java_lang_ThreadGroup::compute_offsets() {
  InstanceKlass* k = SystemDictionary::ThreadGroup_klass();
  THREADGROUP_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

#define THROWABLE_FIELDS_DO(macro) \
  macro(backtrace_offset,     k, "backtrace",     object_signature,                  false); \
  macro(detailMessage_offset, k, "detailMessage", string_signature,                  false); \
  macro(stackTrace_offset,    k, "stackTrace",    java_lang_StackTraceElement_array, false); \
  macro(depth_offset,         k, "depth",         int_signature,                     false); \
  macro(static_unassigned_stacktrace_offset, k, "UNASSIGNED_STACK", java_lang_StackTraceElement_array, true)

void java_lang_Throwable::compute_offsets() {
  InstanceKlass* k = SystemDictionary::Throwable_klass();
  THROWABLE_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

oop java_lang_Throwable::unassigned_stacktrace() {
  InstanceKlass* ik = SystemDictionary::Throwable_klass();
  oop base = ik->static_field_base_raw();
  return base->obj_field(static_unassigned_stacktrace_offset);
}

oop java_lang_Throwable::backtrace(oop throwable) {
  return throwable->obj_field_acquire(backtrace_offset);
}

void java_lang_Throwable::set_backtrace(oop throwable, oop value) {
  throwable->release_obj_field_put(backtrace_offset, value);
}

int java_lang_Throwable::depth(oop throwable) {
  return throwable->int_field(depth_offset);
}

void java_lang_Throwable::set_depth(oop throwable, int value) {
  throwable->int_field_put(depth_offset, value);
}

oop java_lang_Throwable::message(oop throwable) {
  return throwable->obj_field(detailMessage_offset);
}

// Return Symbol for detailed_message or NULL
Symbol* java_lang_Throwable::detail_message(oop throwable) {
  PRESERVE_EXCEPTION_MARK;  // Keep original exception
  oop detailed_message = java_lang_Throwable::message(throwable);
  if (detailed_message != NULL) {
    return java_lang_String::as_symbol(detailed_message, THREAD);
  }
  return NULL;
}

void java_lang_Throwable::set_message(oop throwable, oop value) {
  throwable->obj_field_put(detailMessage_offset, value);
}

void java_lang_Throwable::set_stacktrace(oop throwable, oop st_element_array) {
  throwable->obj_field_put(stackTrace_offset, st_element_array);
}

void java_lang_Throwable::clear_stacktrace(oop throwable) {
  set_stacktrace(throwable, NULL);
}

void java_lang_Throwable::print(oop throwable, outputStream* st) {
  ResourceMark rm;
  Klass* k = throwable->klass();
  st->print("%s", k->external_name());
  oop msg = message(throwable);
  if (msg != NULL) {
    st->print(": %s", java_lang_String::as_utf8_string(msg));
  }
}

// After this many redefines, the stack trace is unreliable.
const int MAX_VERSION = USHRT_MAX;

static inline bool version_matches(Method* method, int version) {
  return method != NULL && (method->constants()->version() == version);
}

// This class provides a simple wrapper over the internal structure of
// exception backtrace to insulate users of the backtrace from needing
// to know what it looks like.
class BacktraceBuilder: public StackObj {
 friend class BacktraceIterator;
 private:
  Handle          _backtrace;
  objArrayOop     _head;
  typeArrayOop    _methods;
  typeArrayOop    _bcis;
  objArrayOop     _mirrors;
  typeArrayOop    _names; // needed to insulate method name against redefinition
  int             _index;
  NoSafepointVerifier _nsv;

  enum {
    trace_methods_offset = java_lang_Throwable::trace_methods_offset,
    trace_bcis_offset    = java_lang_Throwable::trace_bcis_offset,
    trace_mirrors_offset = java_lang_Throwable::trace_mirrors_offset,
    trace_names_offset   = java_lang_Throwable::trace_names_offset,
    trace_next_offset    = java_lang_Throwable::trace_next_offset,
    trace_size           = java_lang_Throwable::trace_size,
    trace_chunk_size     = java_lang_Throwable::trace_chunk_size
  };

  // get info out of chunks
  static typeArrayOop get_methods(objArrayHandle chunk) {
    typeArrayOop methods = typeArrayOop(chunk->obj_at(trace_methods_offset));
    return methods;
  }
  static typeArrayOop get_bcis(objArrayHandle chunk) {
    typeArrayOop bcis = typeArrayOop(chunk->obj_at(trace_bcis_offset));
    return bcis;
  }
  static objArrayOop get_mirrors(objArrayHandle chunk) {
    objArrayOop mirrors = objArrayOop(chunk->obj_at(trace_mirrors_offset));
    return mirrors;
  }
  static typeArrayOop get_names(objArrayHandle chunk) {
    typeArrayOop names = typeArrayOop(chunk->obj_at(trace_names_offset));
    return names;
  }

 public:
  // constructor for new backtrace
  BacktraceBuilder(TRAPS): _methods(NULL), _bcis(NULL), _head(NULL), _mirrors(NULL), _names(NULL) {
    expand(CHECK);
    _backtrace = Handle(THREAD, _head);
    _index = 0;
  }

  BacktraceBuilder(Thread* thread, objArrayHandle backtrace) {
    _methods = get_methods(backtrace);
    _bcis = get_bcis(backtrace);
    _mirrors = get_mirrors(backtrace);
    _names = get_names(backtrace);

    // head is the preallocated backtrace
    _head = backtrace();
    _backtrace = Handle(thread, _head);
    _index = 0;
  }

  void expand(TRAPS) {
    objArrayHandle old_head(THREAD, _head);
    PauseNoSafepointVerifier pnsv(&_nsv);

    objArrayOop head = oopFactory::new_objectArray(trace_size, CHECK);
    objArrayHandle new_head(THREAD, head);

    typeArrayOop methods = oopFactory::new_shortArray(trace_chunk_size, CHECK);
    typeArrayHandle new_methods(THREAD, methods);

    typeArrayOop bcis = oopFactory::new_intArray(trace_chunk_size, CHECK);
    typeArrayHandle new_bcis(THREAD, bcis);

    objArrayOop mirrors = oopFactory::new_objectArray(trace_chunk_size, CHECK);
    objArrayHandle new_mirrors(THREAD, mirrors);

    typeArrayOop names = oopFactory::new_symbolArray(trace_chunk_size, CHECK);
    typeArrayHandle new_names(THREAD, names);

    if (!old_head.is_null()) {
      old_head->obj_at_put(trace_next_offset, new_head());
    }
    new_head->obj_at_put(trace_methods_offset, new_methods());
    new_head->obj_at_put(trace_bcis_offset, new_bcis());
    new_head->obj_at_put(trace_mirrors_offset, new_mirrors());
    new_head->obj_at_put(trace_names_offset, new_names());

    _head    = new_head();
    _methods = new_methods();
    _bcis = new_bcis();
    _mirrors = new_mirrors();
    _names  = new_names();
    _index = 0;
  }

  oop backtrace() {
    return _backtrace();
  }

  inline void push(Method* method, int bci, TRAPS) {
    // Smear the -1 bci to 0 since the array only holds unsigned
    // shorts.  The later line number lookup would just smear the -1
    // to a 0 even if it could be recorded.
    if (bci == SynchronizationEntryBCI) bci = 0;

    if (_index >= trace_chunk_size) {
      methodHandle mhandle(THREAD, method);
      expand(CHECK);
      method = mhandle();
    }

    _methods->ushort_at_put(_index, method->orig_method_idnum());
    _bcis->int_at_put(_index, Backtrace::merge_bci_and_version(bci, method->constants()->version()));

    // Note:this doesn't leak symbols because the mirror in the backtrace keeps the
    // klass owning the symbols alive so their refcounts aren't decremented.
    Symbol* name = method->name();
    _names->symbol_at_put(_index, name);

    // We need to save the mirrors in the backtrace to keep the class
    // from being unloaded while we still have this stack trace.
    _mirrors->obj_at_put(_index, method->method_holder()->java_mirror());
    _index++;
  }
};

struct BacktraceElement : public StackObj {
  int _method_id;
  int _bci;
  int _version;
  Symbol* _name;
  Handle _mirror;
  BacktraceElement(Handle mirror, int mid, int version, int bci, Symbol* name) :
                   _mirror(mirror), _method_id(mid), _version(version), _bci(bci), _name(name) { }
};

class BacktraceIterator : public StackObj {
  int _index;
  objArrayHandle  _result;
  objArrayHandle  _mirrors;
  typeArrayHandle _methods;
  typeArrayHandle _bcis;
  typeArrayHandle _names;

  void init(objArrayHandle result, Thread* thread) {
    // Get method id, bci, version and mirror from chunk
    _result = result;
    if (_result.not_null()) {
      _methods = typeArrayHandle(thread, BacktraceBuilder::get_methods(_result));
      _bcis = typeArrayHandle(thread, BacktraceBuilder::get_bcis(_result));
      _mirrors = objArrayHandle(thread, BacktraceBuilder::get_mirrors(_result));
      _names = typeArrayHandle(thread, BacktraceBuilder::get_names(_result));
      _index = 0;
    }
  }
 public:
  BacktraceIterator(objArrayHandle result, Thread* thread) {
    init(result, thread);
  }

  BacktraceElement next(Thread* thread) {
    BacktraceElement e (Handle(thread, _mirrors->obj_at(_index)),
                        _methods->ushort_at(_index),
                        Backtrace::version_at(_bcis->int_at(_index)),
                        Backtrace::bci_at(_bcis->int_at(_index)),
                        _names->symbol_at(_index));
    _index++;

    if (_index >= java_lang_Throwable::trace_chunk_size) {
      int next_offset = java_lang_Throwable::trace_next_offset;
      // Get next chunk
      objArrayHandle result (thread, objArrayOop(_result->obj_at(next_offset)));
      init(result, thread);
    }
    return e;
  }

  bool repeat() {
    return _result.not_null() && _mirrors->obj_at(_index) != NULL;
  }
};

// Print stack trace element to resource allocated buffer
static void print_stack_element_to_stream(outputStream* st, Handle mirror, int method_id, int version, int bci, Symbol* name) {
  ResourceMark rm;

  // Get strings and string lengths
  InstanceKlass* holder = InstanceKlass::cast(java_lang_Class::as_Klass(mirror()));
  const char* klass_name  = holder->external_name();
  int buf_len = (int)strlen(klass_name);

  char* method_name = name->as_C_string();
  buf_len += (int)strlen(method_name);

  char* source_file_name = NULL;
  Symbol* source = Backtrace::get_source_file_name(holder, version);
  if (source != NULL) {
    source_file_name = source->as_C_string();
    buf_len += (int)strlen(source_file_name);
  }

  char *module_name = NULL, *module_version = NULL;
  ModuleEntry* module = holder->module();
  if (module->is_named()) {
    module_name = module->name()->as_C_string();
    buf_len += (int)strlen(module_name);
    if (module->version() != NULL) {
      module_version = module->version()->as_C_string();
      buf_len += (int)strlen(module_version);
    }
  }

  // Allocate temporary buffer with extra space for formatting and line number
  char* buf = NEW_RESOURCE_ARRAY(char, buf_len + 64);

  // Print stack trace line in buffer
  sprintf(buf, "\tat %s.%s(", klass_name, method_name);

  // Print module information
  if (module_name != NULL) {
    if (module_version != NULL) {
      sprintf(buf + (int)strlen(buf), "%s@%s/", module_name, module_version);
    } else {
      sprintf(buf + (int)strlen(buf), "%s/", module_name);
    }
  }

  // The method can be NULL if the requested class version is gone
  Method* method = holder->method_with_orig_idnum(method_id, version);
  if (!version_matches(method, version)) {
    strcat(buf, "Redefined)");
  } else {
    int line_number = Backtrace::get_line_number(method, bci);
    if (line_number == -2) {
      strcat(buf, "Native Method)");
    } else {
      if (source_file_name != NULL && (line_number != -1)) {
        // Sourcename and linenumber
        sprintf(buf + (int)strlen(buf), "%s:%d)", source_file_name, line_number);
      } else if (source_file_name != NULL) {
        // Just sourcename
        sprintf(buf + (int)strlen(buf), "%s)", source_file_name);
      } else {
        // Neither sourcename nor linenumber
        sprintf(buf + (int)strlen(buf), "Unknown Source)");
      }
      CompiledMethod* nm = method->code();
    }
  }

  st->print_cr("%s", buf);
}

void java_lang_Throwable::print_stack_element(outputStream* st, const methodHandle& method, int bci) {
  Handle mirror (Thread::current(),  method->method_holder()->java_mirror());
  int method_id = method->orig_method_idnum();
  int version = method->constants()->version();
  print_stack_element_to_stream(st, mirror, method_id, version, bci, method->name());
}

/**
 * Print the throwable message and its stack trace plus all causes by walking the
 * cause chain.  The output looks the same as of Throwable.printStackTrace().
 */
void java_lang_Throwable::print_stack_trace(Handle throwable, outputStream* st) {
  // First, print the message.
  print(throwable(), st);
  st->cr();

  // Now print the stack trace.
  Thread* THREAD = Thread::current();
  while (throwable.not_null()) {
    objArrayHandle result (THREAD, objArrayOop(backtrace(throwable())));
    if (result.is_null()) {
      st->print_raw_cr("\t<<no stack trace available>>");
      return;
    }
    BacktraceIterator iter(result, THREAD);

    while (iter.repeat()) {
      BacktraceElement bte = iter.next(THREAD);
      print_stack_element_to_stream(st, bte._mirror, bte._method_id, bte._version, bte._bci, bte._name);
    }
    {
      // Call getCause() which doesn't necessarily return the _cause field.
      EXCEPTION_MARK;
      JavaValue cause(T_OBJECT);
      JavaCalls::call_virtual(&cause,
                              throwable,
                              throwable->klass(),
                              vmSymbols::getCause_name(),
                              vmSymbols::void_throwable_signature(),
                              THREAD);
      // Ignore any exceptions. we are in the middle of exception handling. Same as classic VM.
      if (HAS_PENDING_EXCEPTION) {
        CLEAR_PENDING_EXCEPTION;
        throwable = Handle();
      } else {
        throwable = Handle(THREAD, (oop) cause.get_jobject());
        if (throwable.not_null()) {
          st->print("Caused by: ");
          print(throwable(), st);
          st->cr();
        }
      }
    }
  }
}

/**
 * Print the throwable stack trace by calling the Java method java.lang.Throwable.printStackTrace().
 */
void java_lang_Throwable::java_printStackTrace(Handle throwable, TRAPS) {
  JavaValue result(T_VOID);
  JavaCalls::call_virtual(&result, throwable, SystemDictionary::Throwable_klass(), vmSymbols::printStackTrace_name(), vmSymbols::void_method_signature(), THREAD);
}

void java_lang_Throwable::fill_in_stack_trace(Handle throwable, const methodHandle& method, TRAPS) {
  if (!StackTraceInThrowable) return;
  ResourceMark rm(THREAD);

  // Start out by clearing the backtrace for this object, in case the VM
  // runs out of memory while allocating the stack trace
  set_backtrace(throwable(), NULL);
  // Clear lazily constructed Java level stacktrace if refilling occurs
  // This is unnecessary in 1.7+ but harmless
  clear_stacktrace(throwable());

  int max_depth = MaxJavaStackTraceDepth;
  JavaThread* thread = (JavaThread*)THREAD;

  BacktraceBuilder bt(CHECK);

  // If there is no Java frame just return the method that was being called
  // with bci 0
  if (!thread->has_last_Java_frame()) {
    if (max_depth >= 1 && method() != NULL) {
      bt.push(method(), 0, CHECK);
      set_depth(throwable(), 1);
      set_backtrace(throwable(), bt.backtrace());
    }
    return;
  }

  // Instead of using vframe directly, this version of fill_in_stack_trace
  // basically handles everything by hand. This significantly improved the
  // speed of this method call up to 28.5% on Solaris sparc. 27.1% on Windows.
  // See bug 6333838 for  more details.
  int total_count = 0;
  RegisterMap map(thread, false);
  int decode_offset = 0;
  CompiledMethod* nm = NULL;
  bool skip_fillInStackTrace_check = false;
  bool skip_throwableInit_check = false;
  bool skip_hidden = !ShowHiddenFrames;

  for (frame fr = thread->last_frame(); max_depth == 0 || max_depth != total_count;) {
    Method* method = NULL;
    int bci = 0;

    // Compiled java method case.
    if (decode_offset != 0) {
      DebugInfoReadStream stream(nm, decode_offset);
      decode_offset = stream.read_int();
      method = (Method*)nm->metadata_at(stream.read_int());
      bci = stream.read_bci();
    } else {
      if (fr.is_first_frame()) break;
      address pc = fr.pc();
      {
        CodeBlob* cb = fr.cb();
        // HMMM QQQ might be nice to have frame return nm as NULL if cb is non-NULL
        // but non nmethod
        fr = fr.sender(&map);
        if (cb == NULL || !cb->is_compiled()) {
          continue;
        }
        nm = cb->as_compiled_method();
        if (nm->method()->is_native()) {
          method = nm->method();
          bci = 0;
        } else {
          PcDesc* pd = nm->pc_desc_at(pc);
          decode_offset = pd->scope_decode_offset();
          // if decode_offset is not equal to 0, it will execute the
          // "compiled java method case" at the beginning of the loop.
          continue;
        }
      }
    }

    // the format of the stacktrace will be:
    // - 1 or more fillInStackTrace frames for the exception class (skipped)
    // - 0 or more <init> methods for the exception class (skipped)
    // - rest of the stack

    if (!skip_fillInStackTrace_check) {
      if (method->name() == vmSymbols::fillInStackTrace_name() && throwable->is_a(method->method_holder())) {
        continue;
      } else {
        skip_fillInStackTrace_check = true; // gone past them all
      }
    }
    if (!skip_throwableInit_check) {
      // skip <init> methods of the exception class and superclasses
      // This is simlar to classic VM.
      if (method->name() == vmSymbols::object_initializer_name() && throwable->is_a(method->method_holder())) {
        continue;
      } else {
        // there are none or we've seen them all - either way stop checking
        skip_throwableInit_check = true;
      }
    }
    bt.push(method, bci, CHECK);
    total_count++;
  }

  // Put completed stack trace into throwable object
  set_backtrace(throwable(), bt.backtrace());
  set_depth(throwable(), total_count);
}

void java_lang_Throwable::fill_in_stack_trace(Handle throwable, const methodHandle& method) {
  // No-op if stack trace is disabled
  if (!StackTraceInThrowable) {
    return;
  }

  // Disable stack traces for some preallocated out of memory errors
  if (!Universe::should_fill_in_stack_trace(throwable)) {
    return;
  }

  PRESERVE_EXCEPTION_MARK;

  JavaThread* thread = JavaThread::active();
  fill_in_stack_trace(throwable, method, thread);
  // ignore exceptions thrown during stack trace filling
  CLEAR_PENDING_EXCEPTION;
}

void java_lang_Throwable::allocate_backtrace(Handle throwable, TRAPS) {
  // Allocate stack trace - backtrace is created but not filled in

  // No-op if stack trace is disabled
  if (!StackTraceInThrowable) return;
  BacktraceBuilder bt(CHECK);   // creates a backtrace
  set_backtrace(throwable(), bt.backtrace());
}

void java_lang_Throwable::fill_in_stack_trace_of_preallocated_backtrace(Handle throwable) {
  // Fill in stack trace into preallocated backtrace (no GC)

  // No-op if stack trace is disabled
  if (!StackTraceInThrowable) return;

  JavaThread* THREAD = JavaThread::current();

  objArrayHandle backtrace (THREAD, (objArrayOop)java_lang_Throwable::backtrace(throwable()));

  ResourceMark rm(THREAD);
  vframeStream st(THREAD);

  BacktraceBuilder bt(THREAD, backtrace);

  // Unlike fill_in_stack_trace we do not skip fillInStackTrace or throwable init
  // methods as preallocated errors aren't created by "java" code.

  // fill in as much stack trace as possible
  int chunk_count = 0;
  for (;!st.at_end(); st.next()) {
    bt.push(st.method(), st.bci(), CHECK);
    chunk_count++;

    // Bail-out for deep stacks
    if (chunk_count >= trace_chunk_size) break;
  }
  set_depth(throwable(), chunk_count);

  // We support the Throwable immutability protocol defined for Java 7.
  java_lang_Throwable::set_stacktrace(throwable(), java_lang_Throwable::unassigned_stacktrace());
}

#define ACCESSIBLEOBJECT_FIELDS_DO(macro) \
  macro(override_offset, k, "override", bool_signature, false)

void java_lang_reflect_AccessibleObject::compute_offsets() {
  InstanceKlass* k = SystemDictionary::reflect_AccessibleObject_klass();
  ACCESSIBLEOBJECT_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

jboolean java_lang_reflect_AccessibleObject::override(oop reflect) {
  return (jboolean) reflect->bool_field(override_offset);
}

void java_lang_reflect_AccessibleObject::set_override(oop reflect, jboolean value) {
  reflect->bool_field_put(override_offset, (int) value);
}

#define METHOD_FIELDS_DO(macro) \
  macro(clazz_offset,          k, vmSymbols::clazz_name(),          class_signature,       false); \
  macro(name_offset,           k, vmSymbols::name_name(),           string_signature,      false); \
  macro(returnType_offset,     k, vmSymbols::returnType_name(),     class_signature,       false); \
  macro(parameterTypes_offset, k, vmSymbols::parameterTypes_name(), class_array_signature, false); \
  macro(exceptionTypes_offset, k, vmSymbols::exceptionTypes_name(), class_array_signature, false); \
  macro(slot_offset,           k, vmSymbols::slot_name(),           int_signature,         false); \
  macro(modifiers_offset,      k, vmSymbols::modifiers_name(),      int_signature,         false); \
  macro##_OPTIONAL(signature_offset,             k, vmSymbols::signature_name(),             string_signature); \
  macro##_OPTIONAL(annotations_offset,           k, vmSymbols::annotations_name(),           byte_array_signature); \
  macro##_OPTIONAL(parameter_annotations_offset, k, vmSymbols::parameter_annotations_name(), byte_array_signature); \
  macro##_OPTIONAL(annotation_default_offset,    k, vmSymbols::annotation_default_name(),    byte_array_signature); \
  macro##_OPTIONAL(type_annotations_offset,      k, vmSymbols::type_annotations_name(),      byte_array_signature)

void java_lang_reflect_Method::compute_offsets() {
  InstanceKlass* k = SystemDictionary::reflect_Method_klass();
  // The generic signature and annotations fields are only present in 1.5
  signature_offset = -1;
  annotations_offset = -1;
  parameter_annotations_offset = -1;
  annotation_default_offset = -1;
  type_annotations_offset = -1;
  METHOD_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

Handle java_lang_reflect_Method::create(TRAPS) {
  Klass* klass = SystemDictionary::reflect_Method_klass();
  // This class is eagerly initialized during VM initialization, since we keep a refence
  // to one of the methods
  return InstanceKlass::cast(klass)->allocate_instance_handle(THREAD);
}

oop java_lang_reflect_Method::clazz(oop reflect) {
  return reflect->obj_field(clazz_offset);
}

void java_lang_reflect_Method::set_clazz(oop reflect, oop value) {
   reflect->obj_field_put(clazz_offset, value);
}

int java_lang_reflect_Method::slot(oop reflect) {
  return reflect->int_field(slot_offset);
}

void java_lang_reflect_Method::set_slot(oop reflect, int value) {
  reflect->int_field_put(slot_offset, value);
}

oop java_lang_reflect_Method::name(oop method) {
  return method->obj_field(name_offset);
}

void java_lang_reflect_Method::set_name(oop method, oop value) {
  method->obj_field_put(name_offset, value);
}

oop java_lang_reflect_Method::return_type(oop method) {
  return method->obj_field(returnType_offset);
}

void java_lang_reflect_Method::set_return_type(oop method, oop value) {
  method->obj_field_put(returnType_offset, value);
}

oop java_lang_reflect_Method::parameter_types(oop method) {
  return method->obj_field(parameterTypes_offset);
}

void java_lang_reflect_Method::set_parameter_types(oop method, oop value) {
  method->obj_field_put(parameterTypes_offset, value);
}

oop java_lang_reflect_Method::exception_types(oop method) {
  return method->obj_field(exceptionTypes_offset);
}

void java_lang_reflect_Method::set_exception_types(oop method, oop value) {
  method->obj_field_put(exceptionTypes_offset, value);
}

int java_lang_reflect_Method::modifiers(oop method) {
  return method->int_field(modifiers_offset);
}

void java_lang_reflect_Method::set_modifiers(oop method, int value) {
  method->int_field_put(modifiers_offset, value);
}

bool java_lang_reflect_Method::has_signature_field() {
  return (signature_offset >= 0);
}

oop java_lang_reflect_Method::signature(oop method) {
  return method->obj_field(signature_offset);
}

void java_lang_reflect_Method::set_signature(oop method, oop value) {
  method->obj_field_put(signature_offset, value);
}

bool java_lang_reflect_Method::has_annotations_field() {
  return (annotations_offset >= 0);
}

oop java_lang_reflect_Method::annotations(oop method) {
  return method->obj_field(annotations_offset);
}

void java_lang_reflect_Method::set_annotations(oop method, oop value) {
  method->obj_field_put(annotations_offset, value);
}

bool java_lang_reflect_Method::has_parameter_annotations_field() {
  return (parameter_annotations_offset >= 0);
}

oop java_lang_reflect_Method::parameter_annotations(oop method) {
  return method->obj_field(parameter_annotations_offset);
}

void java_lang_reflect_Method::set_parameter_annotations(oop method, oop value) {
  method->obj_field_put(parameter_annotations_offset, value);
}

bool java_lang_reflect_Method::has_annotation_default_field() {
  return (annotation_default_offset >= 0);
}

oop java_lang_reflect_Method::annotation_default(oop method) {
  return method->obj_field(annotation_default_offset);
}

void java_lang_reflect_Method::set_annotation_default(oop method, oop value) {
  method->obj_field_put(annotation_default_offset, value);
}

bool java_lang_reflect_Method::has_type_annotations_field() {
  return (type_annotations_offset >= 0);
}

oop java_lang_reflect_Method::type_annotations(oop method) {
  return method->obj_field(type_annotations_offset);
}

void java_lang_reflect_Method::set_type_annotations(oop method, oop value) {
  method->obj_field_put(type_annotations_offset, value);
}

#define CONSTRUCTOR_FIELDS_DO(macro) \
  macro(clazz_offset,          k, vmSymbols::clazz_name(),          class_signature,       false); \
  macro(parameterTypes_offset, k, vmSymbols::parameterTypes_name(), class_array_signature, false); \
  macro(exceptionTypes_offset, k, vmSymbols::exceptionTypes_name(), class_array_signature, false); \
  macro(slot_offset,           k, vmSymbols::slot_name(),           int_signature,         false); \
  macro(modifiers_offset,      k, vmSymbols::modifiers_name(),      int_signature,         false); \
  macro##_OPTIONAL(signature_offset,             k, vmSymbols::signature_name(),             string_signature); \
  macro##_OPTIONAL(annotations_offset,           k, vmSymbols::annotations_name(),           byte_array_signature); \
  macro##_OPTIONAL(parameter_annotations_offset, k, vmSymbols::parameter_annotations_name(), byte_array_signature); \
  macro##_OPTIONAL(type_annotations_offset,      k, vmSymbols::type_annotations_name(),      byte_array_signature)

void java_lang_reflect_Constructor::compute_offsets() {
  InstanceKlass* k = SystemDictionary::reflect_Constructor_klass();
  // The generic signature and annotations fields are only present in 1.5
  signature_offset = -1;
  annotations_offset = -1;
  parameter_annotations_offset = -1;
  type_annotations_offset = -1;
  CONSTRUCTOR_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

Handle java_lang_reflect_Constructor::create(TRAPS) {
  Symbol* name = vmSymbols::java_lang_reflect_Constructor();
  Klass* k = SystemDictionary::resolve_or_fail(name, true, CHECK_NH);
  InstanceKlass* ik = InstanceKlass::cast(k);
  // Ensure it is initialized
  ik->initialize(CHECK_NH);
  return ik->allocate_instance_handle(THREAD);
}

oop java_lang_reflect_Constructor::clazz(oop reflect) {
  return reflect->obj_field(clazz_offset);
}

void java_lang_reflect_Constructor::set_clazz(oop reflect, oop value) {
   reflect->obj_field_put(clazz_offset, value);
}

oop java_lang_reflect_Constructor::parameter_types(oop constructor) {
  return constructor->obj_field(parameterTypes_offset);
}

void java_lang_reflect_Constructor::set_parameter_types(oop constructor, oop value) {
  constructor->obj_field_put(parameterTypes_offset, value);
}

oop java_lang_reflect_Constructor::exception_types(oop constructor) {
  return constructor->obj_field(exceptionTypes_offset);
}

void java_lang_reflect_Constructor::set_exception_types(oop constructor, oop value) {
  constructor->obj_field_put(exceptionTypes_offset, value);
}

int java_lang_reflect_Constructor::slot(oop reflect) {
  return reflect->int_field(slot_offset);
}

void java_lang_reflect_Constructor::set_slot(oop reflect, int value) {
  reflect->int_field_put(slot_offset, value);
}

int java_lang_reflect_Constructor::modifiers(oop constructor) {
  return constructor->int_field(modifiers_offset);
}

void java_lang_reflect_Constructor::set_modifiers(oop constructor, int value) {
  constructor->int_field_put(modifiers_offset, value);
}

bool java_lang_reflect_Constructor::has_signature_field() {
  return (signature_offset >= 0);
}

oop java_lang_reflect_Constructor::signature(oop constructor) {
  return constructor->obj_field(signature_offset);
}

void java_lang_reflect_Constructor::set_signature(oop constructor, oop value) {
  constructor->obj_field_put(signature_offset, value);
}

bool java_lang_reflect_Constructor::has_annotations_field() {
  return (annotations_offset >= 0);
}

oop java_lang_reflect_Constructor::annotations(oop constructor) {
  return constructor->obj_field(annotations_offset);
}

void java_lang_reflect_Constructor::set_annotations(oop constructor, oop value) {
  constructor->obj_field_put(annotations_offset, value);
}

bool java_lang_reflect_Constructor::has_parameter_annotations_field() {
  return (parameter_annotations_offset >= 0);
}

oop java_lang_reflect_Constructor::parameter_annotations(oop method) {
  return method->obj_field(parameter_annotations_offset);
}

void java_lang_reflect_Constructor::set_parameter_annotations(oop method, oop value) {
  method->obj_field_put(parameter_annotations_offset, value);
}

bool java_lang_reflect_Constructor::has_type_annotations_field() {
  return (type_annotations_offset >= 0);
}

oop java_lang_reflect_Constructor::type_annotations(oop constructor) {
  return constructor->obj_field(type_annotations_offset);
}

void java_lang_reflect_Constructor::set_type_annotations(oop constructor, oop value) {
  constructor->obj_field_put(type_annotations_offset, value);
}

#define FIELD_FIELDS_DO(macro) \
  macro(clazz_offset,     k, vmSymbols::clazz_name(),     class_signature,  false); \
  macro(name_offset,      k, vmSymbols::name_name(),      string_signature, false); \
  macro(type_offset,      k, vmSymbols::type_name(),      class_signature,  false); \
  macro(slot_offset,      k, vmSymbols::slot_name(),      int_signature,    false); \
  macro(modifiers_offset, k, vmSymbols::modifiers_name(), int_signature,    false); \
  macro##_OPTIONAL(signature_offset,        k, vmSymbols::signature_name(), string_signature); \
  macro##_OPTIONAL(annotations_offset,      k, vmSymbols::annotations_name(),  byte_array_signature); \
  macro##_OPTIONAL(type_annotations_offset, k, vmSymbols::type_annotations_name(),  byte_array_signature)

void java_lang_reflect_Field::compute_offsets() {
  InstanceKlass* k = SystemDictionary::reflect_Field_klass();
  // The generic signature and annotations fields are only present in 1.5
  signature_offset = -1;
  annotations_offset = -1;
  type_annotations_offset = -1;
  FIELD_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

Handle java_lang_reflect_Field::create(TRAPS) {
  Symbol* name = vmSymbols::java_lang_reflect_Field();
  Klass* k = SystemDictionary::resolve_or_fail(name, true, CHECK_NH);
  InstanceKlass* ik = InstanceKlass::cast(k);
  // Ensure it is initialized
  ik->initialize(CHECK_NH);
  return ik->allocate_instance_handle(THREAD);
}

oop java_lang_reflect_Field::clazz(oop reflect) {
  return reflect->obj_field(clazz_offset);
}

void java_lang_reflect_Field::set_clazz(oop reflect, oop value) {
   reflect->obj_field_put(clazz_offset, value);
}

oop java_lang_reflect_Field::name(oop field) {
  return field->obj_field(name_offset);
}

void java_lang_reflect_Field::set_name(oop field, oop value) {
  field->obj_field_put(name_offset, value);
}

oop java_lang_reflect_Field::type(oop field) {
  return field->obj_field(type_offset);
}

void java_lang_reflect_Field::set_type(oop field, oop value) {
  field->obj_field_put(type_offset, value);
}

int java_lang_reflect_Field::slot(oop reflect) {
  return reflect->int_field(slot_offset);
}

void java_lang_reflect_Field::set_slot(oop reflect, int value) {
  reflect->int_field_put(slot_offset, value);
}

int java_lang_reflect_Field::modifiers(oop field) {
  return field->int_field(modifiers_offset);
}

void java_lang_reflect_Field::set_modifiers(oop field, int value) {
  field->int_field_put(modifiers_offset, value);
}

bool java_lang_reflect_Field::has_signature_field() {
  return (signature_offset >= 0);
}

oop java_lang_reflect_Field::signature(oop field) {
  return field->obj_field(signature_offset);
}

void java_lang_reflect_Field::set_signature(oop field, oop value) {
  field->obj_field_put(signature_offset, value);
}

bool java_lang_reflect_Field::has_annotations_field() {
  return (annotations_offset >= 0);
}

oop java_lang_reflect_Field::annotations(oop field) {
  return field->obj_field(annotations_offset);
}

void java_lang_reflect_Field::set_annotations(oop field, oop value) {
  field->obj_field_put(annotations_offset, value);
}

bool java_lang_reflect_Field::has_type_annotations_field() {
  return (type_annotations_offset >= 0);
}

oop java_lang_reflect_Field::type_annotations(oop field) {
  return field->obj_field(type_annotations_offset);
}

void java_lang_reflect_Field::set_type_annotations(oop field, oop value) {
  field->obj_field_put(type_annotations_offset, value);
}

#define CONSTANTPOOL_FIELDS_DO(macro) \
  macro(_oop_offset, k, "constantPoolOop", object_signature, false)

void reflect_ConstantPool::compute_offsets() {
  InstanceKlass* k = SystemDictionary::reflect_ConstantPool_klass();
  // The field is called ConstantPool* in the sun.reflect.ConstantPool class.
  CONSTANTPOOL_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

#define PARAMETER_FIELDS_DO(macro) \
  macro(name_offset,        k, vmSymbols::name_name(),        string_signature, false); \
  macro(modifiers_offset,   k, vmSymbols::modifiers_name(),   int_signature,    false); \
  macro(index_offset,       k, vmSymbols::index_name(),       int_signature,    false); \
  macro(executable_offset,  k, vmSymbols::executable_name(),  executable_signature, false)

void java_lang_reflect_Parameter::compute_offsets() {
  InstanceKlass* k = SystemDictionary::reflect_Parameter_klass();
  PARAMETER_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

Handle java_lang_reflect_Parameter::create(TRAPS) {
  Symbol* name = vmSymbols::java_lang_reflect_Parameter();
  Klass* k = SystemDictionary::resolve_or_fail(name, true, CHECK_NH);
  InstanceKlass* ik = InstanceKlass::cast(k);
  // Ensure it is initialized
  ik->initialize(CHECK_NH);
  return ik->allocate_instance_handle(THREAD);
}

oop java_lang_reflect_Parameter::name(oop param) {
  return param->obj_field(name_offset);
}

void java_lang_reflect_Parameter::set_name(oop param, oop value) {
  param->obj_field_put(name_offset, value);
}

int java_lang_reflect_Parameter::modifiers(oop param) {
  return param->int_field(modifiers_offset);
}

void java_lang_reflect_Parameter::set_modifiers(oop param, int value) {
  param->int_field_put(modifiers_offset, value);
}

int java_lang_reflect_Parameter::index(oop param) {
  return param->int_field(index_offset);
}

void java_lang_reflect_Parameter::set_index(oop param, int value) {
  param->int_field_put(index_offset, value);
}

oop java_lang_reflect_Parameter::executable(oop param) {
  return param->obj_field(executable_offset);
}

void java_lang_reflect_Parameter::set_executable(oop param, oop value) {
  param->obj_field_put(executable_offset, value);
}

int java_lang_Module::loader_offset;
int java_lang_Module::name_offset;
int java_lang_Module::_module_entry_offset = -1;

Handle java_lang_Module::create(Handle loader, Handle module_name, TRAPS) {
  return JavaCalls::construct_new_instance(SystemDictionary::Module_klass(), vmSymbols::java_lang_module_init_signature(), loader, module_name, CHECK_NH);
}

#define MODULE_FIELDS_DO(macro) \
  macro(loader_offset,  k, vmSymbols::loader_name(),  classloader_signature, false); \
  macro(name_offset,    k, vmSymbols::name_name(),    string_signature,      false)

void java_lang_Module::compute_offsets() {
  InstanceKlass* k = SystemDictionary::Module_klass();
  MODULE_FIELDS_DO(FIELD_COMPUTE_OFFSET);
  MODULE_INJECTED_FIELDS(INJECTED_FIELD_COMPUTE_OFFSET);
}

oop java_lang_Module::loader(oop module) {
  return module->obj_field(loader_offset);
}

void java_lang_Module::set_loader(oop module, oop value) {
  module->obj_field_put(loader_offset, value);
}

oop java_lang_Module::name(oop module) {
  return module->obj_field(name_offset);
}

void java_lang_Module::set_name(oop module, oop value) {
  module->obj_field_put(name_offset, value);
}

ModuleEntry* java_lang_Module::module_entry(oop module) {
  ModuleEntry* module_entry = (ModuleEntry*)module->address_field(_module_entry_offset);
  if (module_entry == NULL) {
    // If the inject field containing the ModuleEntry* is null then return the
    // class loader's unnamed module.
    oop loader = java_lang_Module::loader(module);
    Handle h_loader = Handle(Thread::current(), loader);
    ClassLoaderData* loader_cld = SystemDictionary::register_loader(h_loader);
    return loader_cld->unnamed_module();
  }
  return module_entry;
}

void java_lang_Module::set_module_entry(oop module, ModuleEntry* module_entry) {
  module->address_field_put(_module_entry_offset, (address)module_entry);
}

Handle reflect_ConstantPool::create(TRAPS) {
  InstanceKlass* k = SystemDictionary::reflect_ConstantPool_klass();
  // Ensure it is initialized
  k->initialize(CHECK_NH);
  return k->allocate_instance_handle(THREAD);
}

void reflect_ConstantPool::set_cp(oop reflect, ConstantPool* value) {
  oop mirror = value->pool_holder()->java_mirror();
  // Save the mirror to get back the constant pool.
  reflect->obj_field_put(_oop_offset, mirror);
}

ConstantPool* reflect_ConstantPool::get_cp(oop reflect) {
  oop mirror = reflect->obj_field(_oop_offset);
  Klass* k = java_lang_Class::as_Klass(mirror);

  // Get the constant pool back from the klass.  Since class redefinition
  // merges the new constant pool into the old, this is essentially the
  // same constant pool as the original.  If constant pool merging is
  // no longer done in the future, this will have to change to save
  // the original.
  return InstanceKlass::cast(k)->constants();
}

#define UNSAFESTATICFIELDACCESSORIMPL_FIELDS_DO(macro) \
  macro(_base_offset, k, "base", object_signature, false)

void reflect_UnsafeStaticFieldAccessorImpl::compute_offsets() {
  InstanceKlass* k = SystemDictionary::reflect_UnsafeStaticFieldAccessorImpl_klass();
  UNSAFESTATICFIELDACCESSORIMPL_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

oop java_lang_boxing_object::initialize_and_allocate(BasicType type, TRAPS) {
  Klass* k = SystemDictionary::box_klass(type);
  if (k == NULL)  return NULL;
  InstanceKlass* ik = InstanceKlass::cast(k);
  if (!ik->is_initialized())  ik->initialize(CHECK_0);
  return ik->allocate_instance(THREAD);
}

oop java_lang_boxing_object::create(BasicType type, jvalue* value, TRAPS) {
  oop box = initialize_and_allocate(type, CHECK_0);
  if (box == NULL)  return NULL;
  switch (type) {
    case T_BOOLEAN:
      box->bool_field_put(value_offset, value->z);
      break;
    case T_CHAR:
      box->char_field_put(value_offset, value->c);
      break;
    case T_FLOAT:
      box->float_field_put(value_offset, value->f);
      break;
    case T_DOUBLE:
      box->double_field_put(long_value_offset, value->d);
      break;
    case T_BYTE:
      box->byte_field_put(value_offset, value->b);
      break;
    case T_SHORT:
      box->short_field_put(value_offset, value->s);
      break;
    case T_INT:
      box->int_field_put(value_offset, value->i);
      break;
    case T_LONG:
      box->long_field_put(long_value_offset, value->j);
      break;
    default:
      return NULL;
  }
  return box;
}

BasicType java_lang_boxing_object::basic_type(oop box) {
  if (box == NULL)  return T_ILLEGAL;
  BasicType type = SystemDictionary::box_klass_type(box->klass());
  if (type == T_OBJECT)         // 'unknown' value returned by SD::bkt
    return T_ILLEGAL;
  return type;
}

BasicType java_lang_boxing_object::get_value(oop box, jvalue* value) {
  BasicType type = SystemDictionary::box_klass_type(box->klass());
  switch (type) {
  case T_BOOLEAN:
    value->z = box->bool_field(value_offset);
    break;
  case T_CHAR:
    value->c = box->char_field(value_offset);
    break;
  case T_FLOAT:
    value->f = box->float_field(value_offset);
    break;
  case T_DOUBLE:
    value->d = box->double_field(long_value_offset);
    break;
  case T_BYTE:
    value->b = box->byte_field(value_offset);
    break;
  case T_SHORT:
    value->s = box->short_field(value_offset);
    break;
  case T_INT:
    value->i = box->int_field(value_offset);
    break;
  case T_LONG:
    value->j = box->long_field(long_value_offset);
    break;
  default:
    return T_ILLEGAL;
  }
  return type;
}

BasicType java_lang_boxing_object::set_value(oop box, jvalue* value) {
  BasicType type = SystemDictionary::box_klass_type(box->klass());
  switch (type) {
  case T_BOOLEAN:
    box->bool_field_put(value_offset, value->z);
    break;
  case T_CHAR:
    box->char_field_put(value_offset, value->c);
    break;
  case T_FLOAT:
    box->float_field_put(value_offset, value->f);
    break;
  case T_DOUBLE:
    box->double_field_put(long_value_offset, value->d);
    break;
  case T_BYTE:
    box->byte_field_put(value_offset, value->b);
    break;
  case T_SHORT:
    box->short_field_put(value_offset, value->s);
    break;
  case T_INT:
    box->int_field_put(value_offset, value->i);
    break;
  case T_LONG:
    box->long_field_put(long_value_offset, value->j);
    break;
  default:
    return T_ILLEGAL;
  }
  return type;
}

void java_lang_boxing_object::print(BasicType type, jvalue* value, outputStream* st) {
  switch (type) {
  case T_BOOLEAN:   st->print("%s", value->z ? "true" : "false");   break;
  case T_CHAR:      st->print("%d", value->c);                      break;
  case T_BYTE:      st->print("%d", value->b);                      break;
  case T_SHORT:     st->print("%d", value->s);                      break;
  case T_INT:       st->print("%d", value->i);                      break;
  case T_LONG:      st->print(JLONG_FORMAT, value->j);              break;
  case T_FLOAT:     st->print("%f", value->f);                      break;
  case T_DOUBLE:    st->print("%lf", value->d);                     break;
  default:          st->print("type %d?", type);                    break;
  }
}

// Support for java_lang_ref_Reference

bool java_lang_ref_Reference::is_referent_field(oop obj, ptrdiff_t offset) {
  if (offset != java_lang_ref_Reference::referent_offset) {
    return false;
  }

  Klass* k = obj->klass();
  if (!k->is_instance_klass()) {
    return false;
  }

  InstanceKlass* ik = InstanceKlass::cast(obj->klass());
  bool is_reference = ik->reference_type() != REF_NONE;
  return is_reference;
}

// Support for java_lang_ref_SoftReference
//

#define SOFTREFERENCE_FIELDS_DO(macro) \
  macro(timestamp_offset,    k, "timestamp", long_signature, false); \
  macro(static_clock_offset, k, "clock",     long_signature, true)

void java_lang_ref_SoftReference::compute_offsets() {
  InstanceKlass* k = SystemDictionary::SoftReference_klass();
  SOFTREFERENCE_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

jlong java_lang_ref_SoftReference::timestamp(oop ref) {
  return ref->long_field(timestamp_offset);
}

jlong java_lang_ref_SoftReference::clock() {
  InstanceKlass* ik = SystemDictionary::SoftReference_klass();
  oop base = ik->static_field_base_raw();
  return base->long_field(static_clock_offset);
}

void java_lang_ref_SoftReference::set_clock(jlong value) {
  InstanceKlass* ik = SystemDictionary::SoftReference_klass();
  oop base = ik->static_field_base_raw();
  base->long_field_put(static_clock_offset, value);
}

// Support for java_security_AccessControlContext

int java_security_AccessControlContext::_context_offset = 0;
int java_security_AccessControlContext::_privilegedContext_offset = 0;
int java_security_AccessControlContext::_isPrivileged_offset = 0;
int java_security_AccessControlContext::_isAuthorized_offset = -1;

#define ACCESSCONTROLCONTEXT_FIELDS_DO(macro) \
  macro(_context_offset,           k, "context",      protectiondomain_signature, false); \
  macro(_privilegedContext_offset, k, "privilegedContext", accesscontrolcontext_signature, false); \
  macro(_isPrivileged_offset,      k, "isPrivileged", bool_signature, false); \
  macro(_isAuthorized_offset,      k, "isAuthorized", bool_signature, false)

void java_security_AccessControlContext::compute_offsets() {
  InstanceKlass* k = SystemDictionary::AccessControlContext_klass();
  ACCESSCONTROLCONTEXT_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

bool java_security_AccessControlContext::is_authorized(Handle context) {
  return context->bool_field(_isAuthorized_offset) != 0;
}

oop java_security_AccessControlContext::create(objArrayHandle context, bool isPrivileged, Handle privileged_context, TRAPS) {
  // Ensure klass is initialized
  SystemDictionary::AccessControlContext_klass()->initialize(CHECK_0);
  // Allocate result
  oop result = SystemDictionary::AccessControlContext_klass()->allocate_instance(CHECK_0);
  // Fill in values
  result->obj_field_put(_context_offset, context());
  result->obj_field_put(_privilegedContext_offset, privileged_context());
  result->bool_field_put(_isPrivileged_offset, isPrivileged);
  // whitelist AccessControlContexts created by the JVM if present
  if (_isAuthorized_offset != -1) {
    result->bool_field_put(_isAuthorized_offset, true);
  }
  return result;
}

// Support for java_lang_ClassLoader

bool java_lang_ClassLoader::offsets_computed = false;
int  java_lang_ClassLoader::_loader_data_offset = -1;
int  java_lang_ClassLoader::parallelCapable_offset = -1;
int  java_lang_ClassLoader::name_offset = -1;
int  java_lang_ClassLoader::nameAndId_offset = -1;
int  java_lang_ClassLoader::unnamedModule_offset = -1;

ClassLoaderData* java_lang_ClassLoader::loader_data(oop loader) {
  return HeapAccess<>::load_at(loader, _loader_data_offset);
}

ClassLoaderData* java_lang_ClassLoader::cmpxchg_loader_data(ClassLoaderData* new_data, oop loader, ClassLoaderData* expected_data) {
  return HeapAccess<>::atomic_cmpxchg_at(new_data, loader, _loader_data_offset, expected_data);
}

#define CLASSLOADER_FIELDS_DO(macro) \
  macro(parallelCapable_offset, k1, "parallelLockMap",      concurrenthashmap_signature, false); \
  macro(name_offset,            k1, vmSymbols::name_name(), string_signature, false); \
  macro(nameAndId_offset,       k1, "nameAndId",            string_signature, false); \
  macro(unnamedModule_offset,   k1, "unnamedModule",        module_signature, false); \
  macro(parent_offset,          k1, "parent",               classloader_signature, false)

void java_lang_ClassLoader::compute_offsets() {
  offsets_computed = true;

  InstanceKlass* k1 = SystemDictionary::ClassLoader_klass();
  CLASSLOADER_FIELDS_DO(FIELD_COMPUTE_OFFSET);

  CLASSLOADER_INJECTED_FIELDS(INJECTED_FIELD_COMPUTE_OFFSET);
}

oop java_lang_ClassLoader::parent(oop loader) {
  return loader->obj_field(parent_offset);
}

// Returns the name field of this class loader.  If the name field has not
// been set, null will be returned.
oop java_lang_ClassLoader::name(oop loader) {
  return loader->obj_field(name_offset);
}

// Returns the nameAndId field of this class loader. The format is
// as follows:
//   If the defining loader has a name explicitly set then '<loader-name>' @<id>
//   If the defining loader has no name then <qualified-class-name> @<id>
//   If built-in loader, then omit '@<id>' as there is only one instance.
// Use ClassLoader::loader_name_id() to obtain this String as a char*.
oop java_lang_ClassLoader::nameAndId(oop loader) {
  return loader->obj_field(nameAndId_offset);
}

bool java_lang_ClassLoader::isAncestor(oop loader, oop cl) {
  oop acl = loader;
  // This loop taken verbatim from ClassLoader.java:
  do {
    acl = parent(acl);
    if (oopDesc::equals(cl, acl)) {
      return true;
    }
  } while (acl != NULL);
  return false;
}

bool java_lang_ClassLoader::is_instance(oop obj) {
  return obj != NULL && is_subclass(obj->klass());
}

// For class loader classes, parallelCapable defined
// based on non-null field
// Written to by java.lang.ClassLoader, vm only reads this field, doesn't set it
bool java_lang_ClassLoader::parallelCapable(oop class_loader) {
  if (parallelCapable_offset == -1) {
     // Default for backward compatibility is false
     return false;
  }
  return (class_loader->obj_field(parallelCapable_offset) != NULL);
}

bool java_lang_ClassLoader::is_trusted_loader(oop loader) {
  // Fix for 4474172; see evaluation for more details
  loader = non_reflection_class_loader(loader);

  oop cl = SystemDictionary::java_system_loader();
  while (cl != NULL) {
    if (oopDesc::equals(cl, loader)) return true;
    cl = parent(cl);
  }
  return false;
}

// Return true if this is one of the class loaders associated with
// the generated bytecodes for reflection.
bool java_lang_ClassLoader::is_reflection_class_loader(oop loader) {
  if (loader != NULL) {
    Klass* delegating_cl_class = SystemDictionary::reflect_DelegatingClassLoader_klass();
    // This might be null in non-1.4 JDKs
    return (delegating_cl_class != NULL && loader->is_a(delegating_cl_class));
  }
  return false;
}

oop java_lang_ClassLoader::non_reflection_class_loader(oop loader) {
  // See whether this is one of the class loaders associated with
  // the generated bytecodes for reflection, and if so, "magically"
  // delegate to its parent to prevent class loading from occurring
  // in places where applications using reflection didn't expect it.
  if (is_reflection_class_loader(loader)) {
    return parent(loader);
  }
  return loader;
}

oop java_lang_ClassLoader::unnamedModule(oop loader) {
  return loader->obj_field(unnamedModule_offset);
}

// Caller needs ResourceMark.
const char* java_lang_ClassLoader::describe_external(const oop loader) {
  ClassLoaderData *cld = ClassLoaderData::class_loader_data(loader);
  const char* name = cld->loader_name_and_id();

  // bootstrap loader
  if (loader == NULL) {
    return name;
  }

  bool well_known_loader = SystemDictionary::is_system_class_loader(loader) || SystemDictionary::is_platform_class_loader(loader);

  stringStream ss;
  ss.print("%s (instance of %s", name, loader->klass()->external_name());
  if (!well_known_loader) {
    oop pl = java_lang_ClassLoader::parent(loader);
    const char* parentName = "";
    ClassLoaderData *parent_cld = ClassLoaderData::class_loader_data_or_null(pl);
    // The parent loader's ClassLoaderData could be null if it is
    // a delegating class loader that has never defined a class.
    // In this case the loader's name must be obtained via the parent loader's oop.
    if (parent_cld == NULL) {
      oop cl_name_and_id = java_lang_ClassLoader::nameAndId(pl);
      if (cl_name_and_id != NULL) {
        parentName = java_lang_String::as_utf8_string(cl_name_and_id);
      }
    } else {
      parentName = parent_cld->loader_name_and_id();
    }
    if (pl != NULL) {
      ss.print(", child of %s %s", parentName, pl->klass()->external_name());
    } else {
      // bootstrap loader
      ss.print(", child of %s", parentName);
    }
  }
  ss.print(")");

  return ss.as_string();
}

// Support for java_lang_System
//
#define SYSTEM_FIELDS_DO(macro) \
  macro(static_in_offset,  k, "in",  input_stream_signature, true); \
  macro(static_out_offset, k, "out", print_stream_signature, true); \
  macro(static_err_offset, k, "err", print_stream_signature, true); \
  macro(static_security_offset, k, "security", security_manager_signature, true)

void java_lang_System::compute_offsets() {
  InstanceKlass* k = SystemDictionary::System_klass();
  SYSTEM_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

int java_lang_System::in_offset_in_bytes() { return static_in_offset; }
int java_lang_System::out_offset_in_bytes() { return static_out_offset; }
int java_lang_System::err_offset_in_bytes() { return static_err_offset; }

bool java_lang_System::has_security_manager() {
  InstanceKlass* ik = SystemDictionary::System_klass();
  oop base = ik->static_field_base_raw();
  return base->obj_field(static_security_offset) != NULL;
}

int java_lang_Class::_klass_offset;
int java_lang_Class::_array_klass_offset;
int java_lang_Class::_oop_size_offset;
int java_lang_Class::_static_oop_field_count_offset;
int java_lang_Class::_class_loader_offset;
int java_lang_Class::_module_offset;
int java_lang_Class::_protection_domain_offset;
int java_lang_Class::_component_mirror_offset;
int java_lang_Class::_init_lock_offset;
int java_lang_Class::_signers_offset;
int java_lang_Class::_name_offset;
int java_lang_Class::_source_file_offset;
GrowableArray<Klass*>* java_lang_Class::_fixup_mirror_list = NULL;
GrowableArray<Klass*>* java_lang_Class::_fixup_module_field_list = NULL;
int java_lang_Throwable::backtrace_offset;
int java_lang_Throwable::detailMessage_offset;
int java_lang_Throwable::stackTrace_offset;
int java_lang_Throwable::depth_offset;
int java_lang_Throwable::static_unassigned_stacktrace_offset;
int java_lang_reflect_AccessibleObject::override_offset;
int java_lang_reflect_Method::clazz_offset;
int java_lang_reflect_Method::name_offset;
int java_lang_reflect_Method::returnType_offset;
int java_lang_reflect_Method::parameterTypes_offset;
int java_lang_reflect_Method::exceptionTypes_offset;
int java_lang_reflect_Method::slot_offset;
int java_lang_reflect_Method::modifiers_offset;
int java_lang_reflect_Method::signature_offset;
int java_lang_reflect_Method::annotations_offset;
int java_lang_reflect_Method::parameter_annotations_offset;
int java_lang_reflect_Method::annotation_default_offset;
int java_lang_reflect_Method::type_annotations_offset;
int java_lang_reflect_Constructor::clazz_offset;
int java_lang_reflect_Constructor::parameterTypes_offset;
int java_lang_reflect_Constructor::exceptionTypes_offset;
int java_lang_reflect_Constructor::slot_offset;
int java_lang_reflect_Constructor::modifiers_offset;
int java_lang_reflect_Constructor::signature_offset;
int java_lang_reflect_Constructor::annotations_offset;
int java_lang_reflect_Constructor::parameter_annotations_offset;
int java_lang_reflect_Constructor::type_annotations_offset;
int java_lang_reflect_Field::clazz_offset;
int java_lang_reflect_Field::name_offset;
int java_lang_reflect_Field::type_offset;
int java_lang_reflect_Field::slot_offset;
int java_lang_reflect_Field::modifiers_offset;
int java_lang_reflect_Field::signature_offset;
int java_lang_reflect_Field::annotations_offset;
int java_lang_reflect_Field::type_annotations_offset;
int java_lang_reflect_Parameter::name_offset;
int java_lang_reflect_Parameter::modifiers_offset;
int java_lang_reflect_Parameter::index_offset;
int java_lang_reflect_Parameter::executable_offset;
int java_lang_boxing_object::value_offset;
int java_lang_boxing_object::long_value_offset;
int java_lang_ref_Reference::referent_offset;
int java_lang_ref_Reference::queue_offset;
int java_lang_ref_Reference::next_offset;
int java_lang_ref_Reference::discovered_offset;
int java_lang_ref_SoftReference::timestamp_offset;
int java_lang_ref_SoftReference::static_clock_offset;
int java_lang_ClassLoader::parent_offset;
int java_lang_System::static_in_offset;
int java_lang_System::static_out_offset;
int java_lang_System::static_err_offset;
int java_lang_System::static_security_offset;
int java_lang_AssertionStatusDirectives::classes_offset;
int java_lang_AssertionStatusDirectives::classEnabled_offset;
int java_lang_AssertionStatusDirectives::packages_offset;
int java_lang_AssertionStatusDirectives::packageEnabled_offset;
int java_lang_AssertionStatusDirectives::deflt_offset;
int java_nio_Buffer::_limit_offset;
int java_util_concurrent_locks_AbstractOwnableSynchronizer::_owner_offset;
int reflect_ConstantPool::_oop_offset;
int reflect_UnsafeStaticFieldAccessorImpl::_base_offset;

// Support for java Assertions - java_lang_AssertionStatusDirectives.
#define ASSERTIONSTATUSDIRECTIVES_FIELDS_DO(macro) \
  macro(classes_offset,        k, "classes",        string_array_signature, false); \
  macro(classEnabled_offset,   k, "classEnabled",   bool_array_signature, false); \
  macro(packages_offset,       k, "packages",       string_array_signature, false); \
  macro(packageEnabled_offset, k, "packageEnabled", bool_array_signature,   false); \
  macro(deflt_offset,          k, "deflt",          bool_signature,         false)

void java_lang_AssertionStatusDirectives::compute_offsets() {
  InstanceKlass* k = SystemDictionary::AssertionStatusDirectives_klass();
  ASSERTIONSTATUSDIRECTIVES_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

void java_lang_AssertionStatusDirectives::set_classes(oop o, oop val) {
  o->obj_field_put(classes_offset, val);
}

void java_lang_AssertionStatusDirectives::set_classEnabled(oop o, oop val) {
  o->obj_field_put(classEnabled_offset, val);
}

void java_lang_AssertionStatusDirectives::set_packages(oop o, oop val) {
  o->obj_field_put(packages_offset, val);
}

void java_lang_AssertionStatusDirectives::set_packageEnabled(oop o, oop val) {
  o->obj_field_put(packageEnabled_offset, val);
}

void java_lang_AssertionStatusDirectives::set_deflt(oop o, bool val) {
  o->bool_field_put(deflt_offset, val);
}

// Support for intrinsification of java.nio.Buffer.checkIndex
int java_nio_Buffer::limit_offset() {
  return _limit_offset;
}

#define BUFFER_FIELDS_DO(macro) \
  macro(_limit_offset, k, "limit", int_signature, false)

void java_nio_Buffer::compute_offsets() {
  InstanceKlass* k = SystemDictionary::nio_Buffer_klass();
  BUFFER_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

#define AOS_FIELDS_DO(macro) \
  macro(_owner_offset, k, "exclusiveOwnerThread", thread_signature, false)

void java_util_concurrent_locks_AbstractOwnableSynchronizer::compute_offsets() {
  InstanceKlass* k = SystemDictionary::java_util_concurrent_locks_AbstractOwnableSynchronizer_klass();
  AOS_FIELDS_DO(FIELD_COMPUTE_OFFSET);
}

oop java_util_concurrent_locks_AbstractOwnableSynchronizer::get_owner_threadObj(oop obj) {
  return obj->obj_field(_owner_offset);
}

static int member_offset(int hardcoded_offset) {
  return (hardcoded_offset * heapOopSize) + instanceOopDesc::base_offset_in_bytes();
}

// Compute hard-coded offsets
// Invoked before SystemDictionary::initialize, so pre-loaded classes
// are not available to determine the offset_of_static_fields.
void JavaClasses::compute_hard_coded_offsets() {
  // java_lang_boxing_object
  java_lang_boxing_object::value_offset      = member_offset(java_lang_boxing_object::hc_value_offset);
  java_lang_boxing_object::long_value_offset = align_up(member_offset(java_lang_boxing_object::hc_value_offset), BytesPerLong);

  // java_lang_ref_Reference
  java_lang_ref_Reference::referent_offset   = member_offset(java_lang_ref_Reference::hc_referent_offset);
  java_lang_ref_Reference::queue_offset      = member_offset(java_lang_ref_Reference::hc_queue_offset);
  java_lang_ref_Reference::next_offset       = member_offset(java_lang_ref_Reference::hc_next_offset);
  java_lang_ref_Reference::discovered_offset = member_offset(java_lang_ref_Reference::hc_discovered_offset);
}

// Compute non-hard-coded field offsets of all the classes in this file
void JavaClasses::compute_offsets() {
  // java_lang_Class::compute_offsets was called earlier in bootstrap
  java_lang_System::compute_offsets();
  java_lang_ClassLoader::compute_offsets();
  java_lang_Throwable::compute_offsets();
  java_lang_Thread::compute_offsets();
  java_lang_ThreadGroup::compute_offsets();
  java_lang_AssertionStatusDirectives::compute_offsets();
  java_lang_ref_SoftReference::compute_offsets();
  java_security_AccessControlContext::compute_offsets();

  // Initialize reflection classes. The layouts of these classes
  // changed with the new reflection implementation in JDK 1.4, and
  // since the Universe doesn't know what JDK version it is until this
  // point we defer computation of these offsets until now.
  java_lang_reflect_AccessibleObject::compute_offsets();
  java_lang_reflect_Method::compute_offsets();
  java_lang_reflect_Constructor::compute_offsets();
  java_lang_reflect_Field::compute_offsets();
  java_nio_Buffer::compute_offsets();
  reflect_ConstantPool::compute_offsets();
  reflect_UnsafeStaticFieldAccessorImpl::compute_offsets();
  java_lang_reflect_Parameter::compute_offsets();
  java_lang_Module::compute_offsets();
  java_util_concurrent_locks_AbstractOwnableSynchronizer::compute_offsets();

  // generated interpreter code wants to know about the offsets we just computed:
  AbstractAssembler::update_delayed_values();
}

int InjectedField::compute_offset() {
  InstanceKlass* ik = InstanceKlass::cast(klass());
  for (AllFieldStream fs(ik); !fs.done(); fs.next()) {
    if (!may_be_java && !fs.access_flags().is_internal()) {
      // Only look at injected fields
      continue;
    }
    if (fs.name() == name() && fs.signature() == signature()) {
      return fs.offset();
    }
  }
  ResourceMark rm;
  tty->print_cr("Invalid layout of %s at %s/%s%s", ik->external_name(), name()->as_C_string(), signature()->as_C_string(), may_be_java ? " (may_be_java)" : "");
  vm_exit_during_initialization("Invalid layout of preloaded class: use -Xlog:class+load=info to see the origin of the problem class");
  return -1;
}

void javaClasses_init() {
  JavaClasses::compute_offsets();
  JavaClasses::check_offsets();
  FilteredFieldsMap::initialize();  // must be done after computing offsets.
}
