#ifndef SHARE_VM_CODE_PCDESC_HPP
#define SHARE_VM_CODE_PCDESC_HPP

// PcDescs map a physical PC (given as offset from start of nmethod) to
// the corresponding source scope and byte code index.

class CompiledMethod;

class PcDesc {
  friend class VMStructs;
 private:
  int _pc_offset;           // offset from start of nmethod
  int _scope_decode_offset; // offset for scope in nmethod
  int _obj_decode_offset;

  enum {
    PCDESC_reexecute               = 1 << 0,
    PCDESC_is_method_handle_invoke = 1 << 1,
    PCDESC_return_oop              = 1 << 2,
    PCDESC_rethrow_exception       = 1 << 3
  };

  int _flags;

  void set_flag(int mask, bool z) {
    _flags = z ? (_flags | mask) : (_flags & ~mask);
  }

 public:
  int pc_offset() const           { return _pc_offset; }
  int scope_decode_offset() const { return _scope_decode_offset; }
  int obj_decode_offset() const   { return _obj_decode_offset; }

  void set_pc_offset(int x)           { _pc_offset           = x; }
  void set_scope_decode_offset(int x) { _scope_decode_offset = x; }
  void set_obj_decode_offset(int x)   { _obj_decode_offset   = x; }

  // Constructor (only used for static in nmethod.cpp)
  // Also used by ScopeDesc::sender()]
  PcDesc(int pc_offset, int scope_decode_offset, int obj_decode_offset);

  enum {
    // upper and lower exclusive limits real offsets:
    lower_offset_limit = -1,
    upper_offset_limit = (unsigned int)-1 >> 1
  };

  // Flags
  bool     rethrow_exception()              const { return (_flags & PCDESC_rethrow_exception) != 0; }
  void set_rethrow_exception(bool z)              { set_flag(PCDESC_rethrow_exception, z); }
  bool     should_reexecute()              const { return (_flags & PCDESC_reexecute) != 0; }
  void set_should_reexecute(bool z)              { set_flag(PCDESC_reexecute, z); }

  // Does pd refer to the same information as pd?
  bool is_same_info(const PcDesc* pd) {
    return _scope_decode_offset == pd->_scope_decode_offset && _obj_decode_offset == pd->_obj_decode_offset && _flags == pd->_flags;
  }

  bool     is_method_handle_invoke()       const { return (_flags & PCDESC_is_method_handle_invoke) != 0; }
  void set_is_method_handle_invoke(bool z)       { set_flag(PCDESC_is_method_handle_invoke, z); }

  bool     return_oop()                    const { return (_flags & PCDESC_return_oop) != 0; }
  void set_return_oop(bool z)                    { set_flag(PCDESC_return_oop, z); }

  // Returns the real pc
  address real_pc(const CompiledMethod* code) const;

  void print(CompiledMethod* code);
  bool verify(CompiledMethod* code);
};

#endif
