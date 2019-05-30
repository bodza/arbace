#ifndef SHARE_VM_PRIMS_FORTE_HPP
#define SHARE_VM_PRIMS_FORTE_HPP

// Interface to Forte support.

class Forte : AllStatic {
 public:
   static void register_stub(const char* name, address start, address end) { return; }; // register internal VM stub
};

#endif
