#ifndef SHARE_VM_CODE_RELOCINFO_EXT_HPP
#define SHARE_VM_CODE_RELOCINFO_EXT_HPP

// symbolic_Relocation allows to anotate some addresses in the generated code.
//
// This class was initially defined using the last unused relocType. The
// new version tries to limit the impact on open source code changes.
//
// Without compiled code support, symbolic_Relocation need not be a real
// relocation. To avoid using the last unused relocType, the
// symbolic_Relocation::spec(<any symbolic type>) has been replaced
// by additional methods using directly the symbolic type.
//
// Note: the order of the arguments in some methods had to reversed
// to avoid confusion between the relocType enum and the
// symbolic_reference enum.
class symbolic_Relocation : AllStatic {

 public:
  enum symbolic_reference {
    card_table_reference,
    eden_top_reference,
    heap_end_reference,
    polling_page_reference,
    mark_bits_reference,
    mark_mask_reference,
    oop_bits_reference,
    oop_mask_reference,
    debug_string_reference,
    last_symbolic_reference
  };

  // get the new value for a given symbolic type
  static address symbolic_value(symbolic_reference t);
};

#endif
