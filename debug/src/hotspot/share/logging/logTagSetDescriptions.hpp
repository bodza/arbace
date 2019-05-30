#ifndef SHARE_VM_LOGGING_LOGTAGSETDESCRIPTIONS_HPP
#define SHARE_VM_LOGGING_LOGTAGSETDESCRIPTIONS_HPP

class LogTagSet;

struct LogTagSetDescription {
  const LogTagSet* tagset;
  const char* descr;
};

extern struct LogTagSetDescription tagset_descriptions[];

#endif
