#pragma once

#include <stdbool.h>

extern void init_terminal(bool extended);
extern void restore_terminal(void);

#define READ_EOF                        -1
#define READ_NO_DATA                    -2

extern int read_sync(void);
extern int read_async(void);
