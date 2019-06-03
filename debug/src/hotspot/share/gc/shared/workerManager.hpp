#ifndef SHARE_VM_GC_SHARED_WORKERMANAGER_HPP
#define SHARE_VM_GC_SHARED_WORKERMANAGER_HPP

#include "gc/shared/adaptiveSizePolicy.hpp"

class WorkerManager : public AllStatic {
 public:
  // Create additional workers as needed.
  //   active_workers - number of workers being requested for an upcoming
  // parallel task.
  //   total_workers - total number of workers.  This is the maximum
  // number possible.
  //   created_workers - number of workers already created.  This maybe
  // less than, equal to, or greater than active workers.  If greater than
  // or equal to active_workers, nothing is done.
  //   worker_type - type of thread.
  //   initializing - true if this is called to get the initial number of
  // GC workers.
  // If initializing is true, do a vm exit if the workers cannot be created.
  // The initializing = true case is for JVM start up and failing to
  // create all the worker at start should considered a problem so exit.
  // If initializing = false, there are already some number of worker
  // threads and a failure would not be optimal but should not be fatal.
  template <class WorkerType>
  static uint add_workers (WorkerType* holder, uint active_workers, uint total_workers, uint created_workers, os::ThreadType worker_type, bool initializing) {
    uint start = created_workers;
    uint end = MIN2(active_workers, total_workers);
    for (uint worker_id = start; worker_id < end; worker_id += 1) {
      WorkerThread* new_worker = NULL;
      if (initializing || !InjectGCWorkerCreationFailure) {
        new_worker = holder->install_worker(worker_id);
      }
      if (new_worker == NULL || !os::create_thread(new_worker, worker_type)) {
        if (new_worker != NULL) {
           delete new_worker;
        }
        if (initializing) {
          vm_exit_out_of_memory(0, OOM_MALLOC_ERROR, "Cannot create worker GC thread. Out of system resources.");
        }
        break;
      }
      created_workers++;
      os::start_thread(new_worker);
    }

    return created_workers;
  }
};
#endif
