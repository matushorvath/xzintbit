#pragma once

#include <stdbool.h>

extern void init_terminal(bool extended);
extern void restore_terminal(void);
extern void set_read_sync(void);
extern void set_read_async(void);