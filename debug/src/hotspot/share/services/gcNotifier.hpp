#ifndef SHARE_VM_SERVICES_GCNOTIFIER_HPP
#define SHARE_VM_SERVICES_GCNOTIFIER_HPP

#include "memory/allocation.hpp"
#include "services/memoryPool.hpp"
#include "services/memoryService.hpp"
#include "services/memoryManager.hpp"

class GCNotificationRequest : public CHeapObj<mtInternal> {
  friend class GCNotifier;
  GCNotificationRequest *next;
  jlong timestamp;
  GCMemoryManager *gcManager;
  const char *gcAction;
  const char *gcCause;
  GCStatInfo *gcStatInfo;
public:
  GCNotificationRequest(jlong ts, GCMemoryManager *manager, const char*action, const char *cause,GCStatInfo *info) {
    next = NULL;
    timestamp = ts;
    gcManager = manager;
    gcAction = action;
    gcCause = cause;
    gcStatInfo = info;
  }

  ~GCNotificationRequest() {
    delete gcStatInfo;
  }
};

class GCNotifier : public AllStatic {
  friend class ServiceThread;
private:
  static GCNotificationRequest *first_request;
  static GCNotificationRequest *last_request;
  static void addRequest(GCNotificationRequest *request);
  static GCNotificationRequest *getRequest();
  static void sendNotificationInternal(TRAPS);
public:
  static void pushNotification(GCMemoryManager *manager, const char *action, const char *cause);
  static bool has_event();
  static void sendNotification(TRAPS);
};

#endif
