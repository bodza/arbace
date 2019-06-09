#ifndef CPU_X86_VM_JAVAFRAMEANCHOR_X86_HPP
#define CPU_X86_VM_JAVAFRAMEANCHOR_X86_HPP

private:
  // FP value associated with _last_Java_sp:
  intptr_t* volatile        _last_Java_fp;           // pointer is volatile not what it points to

public:
  // Each arch must define reset, save, restore
  // These are used by objects that only care about:
  //  1 - initializing a new state (thread creation, javaCalls)
  //  2 - saving a current state (javaCalls)
  //  3 - restoring an old state (javaCalls)

  void clear(void) {
    // clearing _last_Java_sp must be first
    _last_Java_sp = NULL;
    // fence?
    _last_Java_fp = NULL;
    _last_Java_pc = NULL;
  }

  void copy(JavaFrameAnchor* src) {
    // In order to make sure the transition state is valid for "this"
    // We must clear _last_Java_sp before copying the rest of the new data
    //
    // Hack Alert: Temporary bugfix for 4717480/4721647
    // To act like previous version (pd_cache_state) don't NULL _last_Java_sp
    // unless the value is changing
    //
    if (_last_Java_sp != src->_last_Java_sp)
      _last_Java_sp = NULL;

    _last_Java_fp = src->_last_Java_fp;
    _last_Java_pc = src->_last_Java_pc;
    // Must be last so profiler will always see valid frame if has_last_frame() is true
    _last_Java_sp = src->_last_Java_sp;
  }

  bool walkable(void)                            { return _last_Java_sp != NULL && _last_Java_pc != NULL; }
  void make_walkable(JavaThread* thread);
  void capture_last_Java_pc(void);

  intptr_t* last_Java_sp(void)             const { return _last_Java_sp; }

  address last_Java_pc(void)                     { return _last_Java_pc; }

private:
  static ByteSize last_Java_fp_offset()          { return byte_offset_of(JavaFrameAnchor, _last_Java_fp); }

public:
  void set_last_Java_sp(intptr_t* sp)            { _last_Java_sp = sp; }

  intptr_t*   last_Java_fp(void)                     { return _last_Java_fp; }
  // Assert (last_Java_sp == NULL || fp == NULL)
  void set_last_Java_fp(intptr_t* fp)                { _last_Java_fp = fp; }

#endif
