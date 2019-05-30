#ifndef SHARE_VM_RUNTIME_FLAGS_FLAGSETTING_HPP
#define SHARE_VM_RUNTIME_FLAGS_FLAGSETTING_HPP

#include "memory/allocation.hpp"

// debug flags control various aspects of the VM and are global accessible

// use FlagSetting to temporarily change some debug flag
// e.g. FlagSetting fs(DebugThisAndThat, true);
// restored to previous value upon leaving scope
class FlagSetting : public StackObj {
  bool val;
  bool* flag;
public:
  FlagSetting(bool& fl, bool newValue) { flag = &fl; val = fl; fl = newValue; }
  ~FlagSetting()                       { *flag = val; }
};

class UIntFlagSetting : public StackObj {
  uint val;
  uint* flag;
public:
  UIntFlagSetting(uint& fl, uint newValue) { flag = &fl; val = fl; fl = newValue; }
  ~UIntFlagSetting()                       { *flag = val; }
};

class SizeTFlagSetting : public StackObj {
  size_t val;
  size_t* flag;
public:
  SizeTFlagSetting(size_t& fl, size_t newValue) { flag = &fl; val = fl; fl = newValue; }
  ~SizeTFlagSetting()                           { *flag = val; }
};

// Helper class for temporarily saving the value of a flag during a scope.
template <size_t SIZE>
class FlagGuard {
  unsigned char _value[SIZE];
  void* const _addr;
public:
  FlagGuard(void* flag_addr) : _addr(flag_addr) { memcpy(_value, _addr, SIZE); }
  ~FlagGuard()                                  { memcpy(_addr, _value, SIZE); }
};

#define FLAG_GUARD(f) FlagGuard<sizeof(f)> f ## _guard(&f)

#endif
