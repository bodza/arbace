#ifndef OS_BSD_VM_OS_SHARE_BSD_HPP
#define OS_BSD_VM_OS_SHARE_BSD_HPP

// misc
void handle_unexpected_exception(Thread* thread, int sig, siginfo_t* info, address pc, address adjusted_pc);

#define PROCFILE_LENGTH 128

#endif
