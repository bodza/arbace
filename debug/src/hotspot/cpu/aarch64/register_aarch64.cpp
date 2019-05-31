#include "precompiled.hpp"
#include "register_aarch64.hpp"

const int ConcreteRegisterImpl::max_gpr = RegisterImpl::number_of_registers << 1;

const int ConcreteRegisterImpl::max_fpr = ConcreteRegisterImpl::max_gpr + (FloatRegisterImpl::number_of_registers << 1);

const char* RegisterImpl::name() const {
  const char* names[number_of_registers] = {
    "c_rarg0", "c_rarg1", "c_rarg2", "c_rarg3", "c_rarg4", "c_rarg5", "c_rarg6", "c_rarg7",
    "rscratch1", "rscratch2",
    "r10", "r11", "r12", "r13", "r14", "r15", "r16",
    "r17", "r18", "r19",
    "resp", "rdispatch", "rbcp", "r23", "rlocals", "rmonitors", "rcpool", "rheapbase",
    "rthread", "rfp", "lr", "sp"
  };
  return is_valid() ? names[encoding()] : "noreg";
}

const char* FloatRegisterImpl::name() const {
  const char* names[number_of_registers] = {
    "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7",
    "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15",
    "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",
    "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31"
  };
  return is_valid() ? names[encoding()] : "noreg";
}
