#include "precompiled.hpp"

#include "asm/macroAssembler.hpp"
#include "runtime/icache.hpp"

extern void aarch64TestHook();

void ICacheStubGenerator::generate_icache_flush(ICache::flush_icache_stub_t* flush_icache_stub) {
  // Give anyone who calls this a surprise
  *flush_icache_stub = (ICache::flush_icache_stub_t)NULL;
}

void ICache::initialize() {
  aarch64TestHook();
}
