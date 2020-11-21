#pragma once

#ifdef ICVM_PROFILE

extern void profile_init();
extern void profile_addr(int addr);
extern void profile_value(int value);
extern void profile_inst();
extern void profile_done();

#else

inline void profile_init() {}
inline void profile_addr(int addr) {}
inline void profile_value(int value) {}
inline void profile_inst() {}
inline void profile_done() {}

#endif
