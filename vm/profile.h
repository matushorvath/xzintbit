#pragma once

#include <stdint.h>

#ifdef ICVM_PROFILE

extern void profile_init();
extern void profile_addr(int64_t addr);
extern void profile_inst();
extern void profile_done();

#else

inline void profile_init() {}
inline void profile_addr(int64_t addr) {}
inline void profile_inst() {}
inline void profile_done() {}

#endif
