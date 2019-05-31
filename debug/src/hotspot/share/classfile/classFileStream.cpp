#include "precompiled.hpp"
#include "classfile/classFileStream.hpp"
#include "classfile/classLoader.hpp"
#include "classfile/dictionary.hpp"
#include "classfile/vmSymbols.hpp"
#include "memory/resourceArea.hpp"

const bool ClassFileStream::verify = true;
const bool ClassFileStream::no_verification = false;

void ClassFileStream::truncated_file_error(TRAPS) const {
  THROW_MSG(vmSymbols::java_lang_ClassFormatError(), "Truncated class file");
}

ClassFileStream::ClassFileStream(const u1* buffer,
                                 int length,
                                 const char* source,
                                 bool verify_stream) :
  _buffer_start(buffer),
  _buffer_end(buffer + length),
  _current(buffer),
  _source(source),
  _need_verify(verify_stream) { }

const u1* ClassFileStream::clone_buffer() const {
  u1* const new_buffer_start = NEW_RESOURCE_ARRAY(u1, length());
  memcpy(new_buffer_start, _buffer_start, length());
  return new_buffer_start;
}

const char* const ClassFileStream::clone_source() const {
  const char* const src = source();
  char* source_copy = NULL;
  if (src != NULL) {
    size_t source_len = strlen(src);
    source_copy = NEW_RESOURCE_ARRAY(char, source_len + 1);
    strncpy(source_copy, src, source_len + 1);
  }
  return source_copy;
}

// Caller responsible for ResourceMark
// clone stream with a rewound position
const ClassFileStream* ClassFileStream::clone() const {
  const u1* const new_buffer_start = clone_buffer();
  return new ClassFileStream(new_buffer_start,
                             length(),
                             clone_source(),
                             need_verify());
}

u1 ClassFileStream::get_u1(TRAPS) const {
  if (_need_verify) {
    guarantee_more(1, CHECK_0);
  } else {
  }
  return *_current++;
}

u2 ClassFileStream::get_u2(TRAPS) const {
  if (_need_verify) {
    guarantee_more(2, CHECK_0);
  } else {
  }
  const u1* tmp = _current;
  _current += 2;
  return Bytes::get_Java_u2((address)tmp);
}

u4 ClassFileStream::get_u4(TRAPS) const {
  if (_need_verify) {
    guarantee_more(4, CHECK_0);
  } else {
  }
  const u1* tmp = _current;
  _current += 4;
  return Bytes::get_Java_u4((address)tmp);
}

u8 ClassFileStream::get_u8(TRAPS) const {
  if (_need_verify) {
    guarantee_more(8, CHECK_0);
  } else {
  }
  const u1* tmp = _current;
  _current += 8;
  return Bytes::get_Java_u8((address)tmp);
}

void ClassFileStream::skip_u1(int length, TRAPS) const {
  if (_need_verify) {
    guarantee_more(length, CHECK);
  }
  _current += length;
}

void ClassFileStream::skip_u2(int length, TRAPS) const {
  if (_need_verify) {
    guarantee_more(length * 2, CHECK);
  }
  _current += length * 2;
}

void ClassFileStream::skip_u4(int length, TRAPS) const {
  if (_need_verify) {
    guarantee_more(length * 4, CHECK);
  }
  _current += length * 4;
}

uint64_t ClassFileStream::compute_fingerprint() const {
  int classfile_size = length();
  int classfile_crc = ClassLoader::crc32(0, (const char*)buffer(), length());
  uint64_t fingerprint = (uint64_t(classfile_size) << 32) | uint64_t(uint32_t(classfile_crc));

  return fingerprint;
}
