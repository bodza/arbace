#include "precompiled.hpp"

#include "classfile/stringTable.hpp"
#include "gc/shared/stringdedup/stringDedup.hpp"
#include "gc/shared/stringdedup/stringDedupQueue.hpp"
#include "gc/shared/stringdedup/stringDedupQueue.inline.hpp"
#include "gc/shared/stringdedup/stringDedupTable.hpp"
#include "gc/shared/stringdedup/stringDedupThread.hpp"
#include "gc/shared/suspendibleThreadSet.hpp"
#include "logging/log.hpp"
#include "oops/access.inline.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/atomic.hpp"

StringDedupThread* StringDedupThread::_thread = NULL;

StringDedupThread::StringDedupThread() :
  ConcurrentGCThread() {
  set_name("StrDedup");
  create_and_start();
}

StringDedupThread::~StringDedupThread() {
  ShouldNotReachHere();
}

StringDedupThread* StringDedupThread::thread() {
  return _thread;
}

class StringDedupSharedClosure: public OopClosure {
 private:
  StringDedupStat* _stat;

 public:
  StringDedupSharedClosure(StringDedupStat* stat) : _stat(stat) { }

  virtual void do_oop(oop* p) { ShouldNotReachHere(); }
  virtual void do_oop(narrowOop* p) {
    oop java_string = RawAccess<>::oop_load(p);
    StringDedupTable::deduplicate(java_string, _stat);
  }
};

// The CDS archive does not include the string dedupication table. Only the string
// table is saved in the archive. The shared strings from CDS archive need to be
// added to the string dedupication table before deduplication occurs. That is
// done in the begining of the StringDedupThread (see StringDedupThread::do_deduplication()).
void StringDedupThread::deduplicate_shared_strings(StringDedupStat* stat) {
  StringDedupSharedClosure sharedStringDedup(stat);
  StringTable::shared_oops_do(&sharedStringDedup);
}

void StringDedupThread::stop_service() {
  StringDedupQueue::cancel_wait();
}

void StringDedupThread::print_start(const StringDedupStat* last_stat) {
  StringDedupStat::print_start(last_stat);
}

void StringDedupThread::print_end(const StringDedupStat* last_stat, const StringDedupStat* total_stat) {
  StringDedupStat::print_end(last_stat, total_stat);
}
