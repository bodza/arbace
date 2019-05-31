#ifndef SHARE_VM_CODE_LOCATION_HPP
#define SHARE_VM_CODE_LOCATION_HPP

#include "asm/assembler.hpp"
#include "code/vmreg.hpp"

// A Location describes a concrete machine variable location
// (such as integer or floating point register or a stack-held
// variable). Used when generating debug-information for nmethods.
//
// Encoding:
//
// bits (use low bits for best compression):
//  Type:   [3..0]
//  Where:  [4]
//  Offset: [31..5]

class Location {
  friend class VMStructs;
 public:
  enum Where {
    on_stack,
    in_register
  };

  enum Type {
    invalid,                    // Invalid location
    normal,                     // Ints, floats, double halves
    oop,                        // Oop (please GC me!)
    int_in_long,                // Integer held in long register
    lng,                        // Long held in one register
    float_in_dbl,               // Float held in double register
    dbl,                        // Double held in one register
    addr,                       // JSR return address
    narrowoop                   // Narrow Oop (please GC me!)
  };

 private:
  enum {
    TYPE_MASK    = (juint) 0x0F,
    TYPE_SHIFT   = 0,
    WHERE_MASK   = (juint) 0x10,
    WHERE_SHIFT  = 4,
    OFFSET_MASK  = (juint) 0xFFFFFFE0,
    OFFSET_SHIFT = 5
  };

  juint _value;

  // Create a bit-packed Location
  Location(Where where_, Type type_, unsigned offset_) {
    set(where_, type_, offset_);
  }

  inline void set(Where where_, Type type_, unsigned offset_) {
    _value = (juint) ((where_  << WHERE_SHIFT) |
                      (type_   << TYPE_SHIFT)  |
                      ((offset_ << OFFSET_SHIFT) & OFFSET_MASK));
  }

 public:

  // Stack location Factory.  Offset is 4-byte aligned; remove low bits
  static Location new_stk_loc( Type t, int offset ) { return Location(on_stack,t,offset>>LogBytesPerInt); }
  // Register location Factory
  static Location new_reg_loc( Type t, VMReg reg ) { return Location(in_register, t, reg->value()); }
  // Default constructor
  Location() { set(on_stack,invalid,0); }

  // Bit field accessors
  Where where()  const { return (Where)       ((_value & WHERE_MASK)  >> WHERE_SHIFT); }
  Type  type()   const { return (Type)        ((_value & TYPE_MASK)   >> TYPE_SHIFT); }
  unsigned offset() const { return (unsigned) ((_value & OFFSET_MASK) >> OFFSET_SHIFT); }

  // Accessors
  bool is_register() const    { return where() == in_register; }
  bool is_stack() const       { return where() == on_stack; }

  int stack_offset() const    {
    return offset()<<LogBytesPerInt; }
  int register_number() const {
    return offset()   ; }

  VMReg reg() const {
    return VMRegImpl::as_VMReg(offset())   ; }

  // Printing
  void print_on(outputStream* st) const;

  // Serialization of debugging information
  Location(DebugInfoReadStream* stream);
  void write_on(DebugInfoWriteStream* stream);

  // check
  static bool legal_offset_in_bytes(int offset_in_bytes);
};

#endif
