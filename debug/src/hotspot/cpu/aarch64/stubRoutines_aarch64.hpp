#ifndef CPU_AARCH64_VM_STUBROUTINES_AARCH64_HPP
#define CPU_AARCH64_VM_STUBROUTINES_AARCH64_HPP

// This file holds the platform specific parts of the StubRoutines
// definition. See stubRoutines.hpp for a description on how to
// extend it.

// n.b. if we are notifying entry/exit to the simulator then the call
// stub does a notify at normal return placing
// call_stub_return_address one instruction beyond the notify. the
// latter address is sued by the stack unwind code when doign an
// exception return.
static bool returns_to_call_stub(address return_pc) {
  return return_pc == _call_stub_return_address + (NotifySimulator ? -4 : 0);
}

enum platform_dependent_constants {
  code_size1 = 19000,          // simply increase if too small (assembler will crash if too small)
  code_size2 = 28000           // simply increase if too small (assembler will crash if too small)
};

class aarch64 {
 friend class StubGenerator;

 private:
  static address _get_previous_fp_entry;
  static address _get_previous_sp_entry;

  static address _f2i_fixup;
  static address _f2l_fixup;
  static address _d2i_fixup;
  static address _d2l_fixup;

  static address _float_sign_mask;
  static address _float_sign_flip;
  static address _double_sign_mask;
  static address _double_sign_flip;

  static address _zero_blocks;

  static address _has_negatives;
  static address _has_negatives_long;
  static address _large_array_equals;
  static address _compare_long_string_LL;
  static address _compare_long_string_LU;
  static address _compare_long_string_UL;
  static address _compare_long_string_UU;
  static address _string_indexof_linear_ll;
  static address _string_indexof_linear_uu;
  static address _string_indexof_linear_ul;
  static address _large_byte_array_inflate;
  static bool _completed;

 public:

  static address get_previous_fp_entry()
  {
    return _get_previous_fp_entry;
  }

  static address get_previous_sp_entry()
  {
    return _get_previous_sp_entry;
  }

  static address f2i_fixup()
  {
    return _f2i_fixup;
  }

  static address f2l_fixup()
  {
    return _f2l_fixup;
  }

  static address d2i_fixup()
  {
    return _d2i_fixup;
  }

  static address d2l_fixup()
  {
    return _d2l_fixup;
  }

  static address float_sign_mask()
  {
    return _float_sign_mask;
  }

  static address float_sign_flip()
  {
    return _float_sign_flip;
  }

  static address double_sign_mask()
  {
    return _double_sign_mask;
  }

  static address double_sign_flip()
  {
    return _double_sign_flip;
  }

  static address zero_blocks() {
    return _zero_blocks;
  }

  static address has_negatives() {
    return _has_negatives;
  }

  static address has_negatives_long() {
      return _has_negatives_long;
  }

  static address large_array_equals() {
      return _large_array_equals;
  }

  static address compare_long_string_LL() {
      return _compare_long_string_LL;
  }

  static address compare_long_string_LU() {
      return _compare_long_string_LU;
  }

  static address compare_long_string_UL() {
      return _compare_long_string_UL;
  }

  static address compare_long_string_UU() {
      return _compare_long_string_UU;
  }

  static address string_indexof_linear_ul() {
      return _string_indexof_linear_ul;
  }

  static address string_indexof_linear_ll() {
      return _string_indexof_linear_ll;
  }

  static address string_indexof_linear_uu() {
      return _string_indexof_linear_uu;
  }

  static address large_byte_array_inflate() {
      return _large_byte_array_inflate;
  }

  static bool complete() {
    return _completed;
  }

  static void set_completed() {
    _completed = true;
  }

private:
  static juint    _crc_table[];
  // begin trigonometric tables block. See comments in .cpp file
  static juint    _npio2_hw[];
  static jdouble   _two_over_pi[];
  static jdouble   _pio2[];
  static jdouble   _dsin_coef[];
  static jdouble  _dcos_coef[];
  // end trigonometric tables block
};

#endif
