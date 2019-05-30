#ifndef SHARE_VM_CI_CIREPLAY_HPP
#define SHARE_VM_CI_CIREPLAY_HPP

#include "ci/ciMethod.hpp"

// ciReplay

//
// Replay compilation of a java method by using an information in replay file.
// Replay inlining decisions during compilation by using an information in inline file.
//
// NOTE: these replay functions only exist in debug version of VM.
//
// Replay compilation.
// -------------------
//
// Replay data file replay.txt can be created by Serviceability Agent
// from a core file, see agent/doc/cireplay.html
//
// $ java -cp <jdk>/lib/sa-jdi.jar sun.jvm.hotspot.CLHSDB
// hsdb> attach <jdk>/bin/java ./core
// hsdb> threads
// t@10 Service Thread
// t@9 C2 CompilerThread0
// t@8 Signal Dispatcher
// t@7 Finalizer
// t@6 Reference Handler
// t@2 main
// hsdb> dumpreplaydata t@9 > replay.txt
// hsdb> quit
//
// (Note: SA could be also used to extract app.jar and boot.jar files
//  from core file to replay compilation if only core file is available)
//
// Replay data file replay_pid%p.log is also created when VM crashes
// in Compiler thread during compilation. It is controlled by
// DumpReplayDataOnError flag which is ON by default.
//
// Replay file replay_pid%p_compid%d.log can be created
// for the specified java method during normal execution using
// CompileCommand option DumpReplay:
//
// -XX:CompileCommand=option,Benchmark::test,DumpReplay
//
// In this case the file name has additional compilation id "_compid%d"
// because the method could be compiled several times.
//
// To replay compilation the replay file should be specified:
//
// -XX:+ReplayCompiles -XX:ReplayDataFile=replay_pid2133.log
//
// VM thread reads data from the file immediately after VM initialization
// and puts the compilation task on compile queue. After that it goes into
// wait state (BackgroundCompilation flag is set to false) since there is no
// a program to execute. VM exits when the compilation is finished.
//
//
// Replay inlining.
// ----------------
//
// Replay inlining file inline_pid%p_compid%d.log is created for
// a specific java method during normal execution of a java program
// using CompileCommand option DumpInline:
//
// -XX:CompileCommand=option,Benchmark::test,DumpInline
//
// To replay inlining the replay file and the method should be specified:
//
// -XX:CompileCommand=option,Benchmark::test,ReplayInline -XX:InlineDataFile=inline_pid3244_compid6.log
//
// The difference from replay compilation is that replay inlining
// is performed during normal java program execution.
//

class ciReplay {
  CI_PACKAGE_ACCESS
};

#endif
