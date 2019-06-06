#include "precompiled.hpp"

#include "runtime/frame.inline.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/thread.inline.hpp"

// Implementation of the platform-specific part of StubRoutines - for
// a description of how to extend it, see the stubRoutines.hpp file.

address StubRoutines::x86::_get_previous_fp_entry = NULL;
address StubRoutines::x86::_get_previous_sp_entry = NULL;

address StubRoutines::x86::_f2i_fixup = NULL;
address StubRoutines::x86::_f2l_fixup = NULL;
address StubRoutines::x86::_d2i_fixup = NULL;
address StubRoutines::x86::_d2l_fixup = NULL;
address StubRoutines::x86::_float_sign_mask = NULL;
address StubRoutines::x86::_float_sign_flip = NULL;
address StubRoutines::x86::_double_sign_mask = NULL;
address StubRoutines::x86::_double_sign_flip = NULL;
