#include "precompiled.hpp"
#include "code/compressedStream.hpp"
#include "utilities/ostream.hpp"

// 32-bit one-to-one sign encoding taken from Pack200
// converts leading sign bits into leading zeroes with trailing sign bit
inline juint CompressedStream::encode_sign(jint  value) {
  return (value << 1) ^ (value >> 31);
}
inline jint  CompressedStream::decode_sign(juint value) {
  return (value >> 1) ^ -(jint)(value & 1);
}

// 32-bit self-inverse encoding of float bits
// converts trailing zeroes (common in floats) to leading zeroes
inline juint CompressedStream::reverse_int(juint i) {
  // Hacker's Delight, Figure 7-1
  i = (i & 0x55555555) << 1 | ((i >> 1) & 0x55555555);
  i = (i & 0x33333333) << 2 | ((i >> 2) & 0x33333333);
  i = (i & 0x0f0f0f0f) << 4 | ((i >> 4) & 0x0f0f0f0f);
  i = (i << 24) | ((i & 0xff00) << 8) | ((i >> 8) & 0xff00) | (i >> 24);
  return i;
}

jint CompressedReadStream::read_signed_int() {
  return decode_sign(read_int());
}

// Compressing floats is simple, because the only common pattern
// is trailing zeroes.  (Compare leading sign bits on ints.)
// Since floats are left-justified, as opposed to right-justified
// ints, we can bit-reverse them in order to take advantage of int
// compression.

jfloat CompressedReadStream::read_float() {
  int rf = read_int();
  int f  = reverse_int(rf);
  return jfloat_cast(f);
}

jdouble CompressedReadStream::read_double() {
  jint rh = read_int();
  jint rl = read_int();
  jint h  = reverse_int(rh);
  jint l  = reverse_int(rl);
  return jdouble_cast(jlong_from(h, l));
}

jlong CompressedReadStream::read_long() {
  jint low  = read_signed_int();
  jint high = read_signed_int();
  return jlong_from(high, low);
}

CompressedWriteStream::CompressedWriteStream(int initial_size) : CompressedStream(NULL, 0) {
  _buffer   = NEW_RESOURCE_ARRAY(u_char, initial_size);
  _size     = initial_size;
  _position = 0;
}

void CompressedWriteStream::grow() {
  u_char* _new_buffer = NEW_RESOURCE_ARRAY(u_char, _size * 2);
  memcpy(_new_buffer, _buffer, _position);
  _buffer = _new_buffer;
  _size   = _size * 2;
}

void CompressedWriteStream::write_signed_int(jint value) {
  // this encoding, called SIGNED5, is taken from Pack200
  write_int(encode_sign(value));
}

void CompressedWriteStream::write_float(jfloat value) {
  juint f = jint_cast(value);
  juint rf = reverse_int(f);
  assert(f == reverse_int(rf), "can re-read same bits");
  write_int(rf);
}

void CompressedWriteStream::write_double(jdouble value) {
  juint h  = high(jlong_cast(value));
  juint l  = low( jlong_cast(value));
  juint rh = reverse_int(h);
  juint rl = reverse_int(l);
  assert(h == reverse_int(rh), "can re-read same bits");
  assert(l == reverse_int(rl), "can re-read same bits");
  write_int(rh);
  write_int(rl);
}

void CompressedWriteStream::write_long(jlong value) {
  write_signed_int(low(value));
  write_signed_int(high(value));
}

/// The remaining details

void CompressedWriteStream::write_int_mb(jint value) {
  juint sum = value;
  for (int i = 0; ; ) {
    if (sum < L || i == MAX_i) {
      // remainder is either a "low code" or the 5th byte
      assert(sum == (u_char)sum, "valid byte");
      write((u_char)sum);
      break;
    }
    sum -= L;
    int b_i = L + (sum % H);  // this is a "high code"
    sum >>= lg_H;             // extracted 6 bits
    write(b_i); ++i;
  }
}
