#ifndef CPU_AARCH64_VM_FRAME_AARCH64_HPP
#define CPU_AARCH64_VM_FRAME_AARCH64_HPP

#include "runtime/synchronizer.hpp"

// A frame represents a physical stack frame (an activation).  Frames can be
// C or Java frames, and the Java frames can be interpreted or compiled.
// In contrast, vframes represent source-level activations, so that one physical frame
// can correspond to multiple source level frames because of inlining.
// A frame is comprised of {pc, fp, sp}
// ------------------------------ Asm interpreter ----------------------------------------
// Layout of asm interpreter frame:
//    [expression stack      ] * <- sp

//    [monitors[0]           ] \
//     ...                        | monitor block size = k
//    [monitors[k-1]         ]   /
//    [frame initial esp     ] ( == &monitors[0], initially here)       initial_sp_offset
//    [byte code index/pointr]                   = bcx()                bcx_offset

//    [pointer to locals     ]                   = locals()             locals_offset
//    [constant pool cache   ]                   = cache()              cache_offset

//    [klass of method       ]                   = mirror()             mirror_offset
//    [padding               ]

//    [methodData            ]                   = mdp()                mdx_offset
//    [methodOop             ]                   = method()             method_offset

//    [last esp              ]                   = last_sp()            last_sp_offset
//    [old stack pointer     ]                     (sender_sp)          sender_sp_offset

//    [old frame pointer     ]   <- fp           = link()
//    [return pc             ]

//    [last sp               ]
//    [oop temp              ]                     (only for native calls)

//    [locals and parameters ]
//                               <- sender sp
// ------------------------------ Asm interpreter ----------------------------------------

 public:
  enum {
    pc_return_offset                                 =  0,
    // All frames
    link_offset                                      =  0,
    return_addr_offset                               =  1,
    sender_sp_offset                                 =  2,

    // Entry frames
    // n.b. these values are determined by the layout defined in
    // stubGenerator for the Java call stub
    entry_frame_after_call_words                     = 27,
    entry_frame_call_wrapper_offset                  = -8,

    // we don't need a save area
    arg_reg_save_area_bytes                          =  0
  };

  intptr_t ptr_at(int offset) const {
    return *ptr_at_addr(offset);
  }

  void ptr_at_put(int offset, intptr_t value) {
    *ptr_at_addr(offset) = value;
  }

 private:
  // an additional field beyond _sp and _pc:
  intptr_t*   _fp; // frame pointer
  // The interpreter and adapters will extend the frame of the caller.
  // Since oopMaps are based on the sp of the caller before extension
  // we need to know that value. However in order to compute the address
  // of the return address we need the real "raw" sp. Since sparc already
  // uses sp() to mean "raw" sp and unextended_sp() to mean the caller's
  // original sp we use that convention.

  intptr_t*     _unextended_sp;
  void adjust_unextended_sp();

  intptr_t* ptr_at_addr(int offset) const {
    return (intptr_t*) addr_at(offset);
  }

 public:
  // Constructors

  frame(intptr_t* sp, intptr_t* fp, address pc);

  frame(intptr_t* sp, intptr_t* unextended_sp, intptr_t* fp, address pc);

  frame(intptr_t* sp, intptr_t* fp);

  void init(intptr_t* sp, intptr_t* fp, address pc);

  // accessors for the instance variables
  // Note: not necessarily the real 'frame pointer' (see real_fp)
  intptr_t* fp() const { return _fp; }

  inline address* sender_pc_addr() const;

  // helper to update a map with callee-saved RBP
  static void update_map_with_saved_link(RegisterMap* map, intptr_t** link_addr);

  static jint interpreter_frame_expression_stack_direction() { return -1; }

#endif
