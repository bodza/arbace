#include "precompiled.hpp"

#include "gc/shared/stringdedup/stringDedup.hpp"
#include "gc/shared/stringdedup/stringDedupQueue.hpp"
#include "gc/shared/stringdedup/stringDedupTable.hpp"
#include "gc/shared/stringdedup/stringDedupThread.hpp"

bool StringDedup::_enabled = false;

void StringDedup::gc_prologue(bool resize_and_rehash_table) {
  assert(is_enabled(), "String deduplication not enabled");
  StringDedupQueue::gc_prologue();
  StringDedupTable::gc_prologue(resize_and_rehash_table);
}
void StringDedup::gc_epilogue() {
  assert(is_enabled(), "String deduplication not enabled");
  StringDedupQueue::gc_epilogue();
  StringDedupTable::gc_epilogue();
}

void StringDedup::stop() {
  assert(is_enabled(), "String deduplication not enabled");
  StringDedupThread::thread()->stop();
}

void StringDedup::deduplicate(oop java_string) {
  assert(is_enabled(), "String deduplication not enabled");
  StringDedupStat dummy; // Statistics from this path is never used
  StringDedupTable::deduplicate(java_string, &dummy);
}

void StringDedup::parallel_unlink(StringDedupUnlinkOrOopsDoClosure* unlink, uint worker_id) {
  assert(is_enabled(), "String deduplication not enabled");
  StringDedupQueue::unlink_or_oops_do(unlink);
  StringDedupTable::unlink_or_oops_do(unlink, worker_id);
}

void StringDedup::threads_do(ThreadClosure* tc) {
  assert(is_enabled(), "String deduplication not enabled");
  tc->do_thread(StringDedupThread::thread());
}

void StringDedup::print_worker_threads_on(outputStream* st) {
  assert(is_enabled(), "String deduplication not enabled");
  StringDedupThread::thread()->print_on(st);
  st->cr();
}

void StringDedup::verify() {
  assert(is_enabled(), "String deduplication not enabled");
  StringDedupQueue::verify();
  StringDedupTable::verify();
}

StringDedupUnlinkOrOopsDoClosure::StringDedupUnlinkOrOopsDoClosure(BoolObjectClosure* is_alive,
                                                                   OopClosure* keep_alive) :
  _is_alive(is_alive), _keep_alive(keep_alive) {
}
