#ifndef SHARE_VM_GC_SHARED_STRINGDEDUP_STRINGDEDUPTHREAD_HPP
#define SHARE_VM_GC_SHARED_STRINGDEDUP_STRINGDEDUPTHREAD_HPP

#include "gc/shared/concurrentGCThread.hpp"
#include "gc/shared/stringdedup/stringDedupStat.hpp"

//
// The deduplication thread is where the actual deduplication occurs. It waits for
// deduplication candidates to appear on the deduplication queue, removes them from
// the queue and tries to deduplicate them. It uses the deduplication hashtable to
// find identical, already existing, character arrays on the heap. The thread runs
// concurrently with the Java application but participates in safepoints to allow
// the GC to adjust and unlink oops from the deduplication queue and table.
//
class StringDedupThread: public ConcurrentGCThread {
protected:
  static StringDedupThread* _thread;

  StringDedupThread();
  ~StringDedupThread();

  void print_start(const StringDedupStat* last_stat);
  void print_end(const StringDedupStat* last_stat, const StringDedupStat* total_stat);

  void run_service() { this->do_deduplication(); }
  void stop_service();

  void deduplicate_shared_strings(StringDedupStat* stat);
protected:
  virtual void do_deduplication() = 0;

public:
  static StringDedupThread* thread();
};

template <typename S>
class StringDedupThreadImpl : public StringDedupThread {
private:
  StringDedupThreadImpl() { }

protected:
  void do_deduplication();

public:
  static void create();
};

#endif
