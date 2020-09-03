#ifdef ICVM_PROFILE

#include <stdio.h>

#include "profile.h"

static int max_mem_size = 0;
static int inst_count = 0;

void profile_init() {
    max_mem_size = 0;
    inst_count = 0;
}

void profile_addr(int addr) {
    if (addr >= max_mem_size) {
        max_mem_size = addr + 1;
    }
}

void profile_inst() {
    inst_count++;
}

void profile_done() {
    fprintf(stderr, "profile: max_mem_size = %d inst_count = %d\n",
        max_mem_size, inst_count);

    FILE* fp = fopen("icvm_profile.csv", "at");
    fprintf(fp, "%d, %d\n", max_mem_size, inst_count);
    fclose(fp);
}

#endif
