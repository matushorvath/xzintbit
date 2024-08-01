#define _POSIX_C_SOURCE 200809L

#include "profile.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

#ifndef WIN32
#   include <signal.h>
#endif // _WIN32

int *profile = NULL;
int profile_size = 0;
char *profile_base_name = NULL;

void init_profile(char *program_name) {
    profile_base_name = program_name;

    profile_size = 64;
    profile = (int *)malloc(profile_size * sizeof(int));
    memset(profile, 0, profile_size * sizeof(int));
}

char *get_profile_name(void) {
    static char profile_name[256];

    time_t t = time(NULL);
    struct tm tm = {};
    localtime_r(&t, &tm);

    sprintf(profile_name, "%s.%02i%02i%02i.%02i%02i%02i.profile.yaml",
        profile_base_name, tm.tm_year - 100, tm.tm_mon + 1, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec);

    return profile_name;
}

void save_profile(void) {
    if (profile != NULL) {
        FILE *fprofile = fopen(get_profile_name(), "wt");

        for (int i = 0; i < profile_size; i++) {
            if (profile[i] != 0) {
                fprintf(fprofile, "%i: %i\n", i, profile[i]);
            }
        }

        fclose(fprofile);

        free(profile);

        profile_size = 64;
        profile = (int *)malloc(profile_size * sizeof(int));
        memset(profile, 0, profile_size * sizeof(int));
    }
}

void resize_profile(int addr) {
    if (profile != NULL && addr >= profile_size) {
        int old_profile_size = profile_size;
        while (addr >= profile_size) profile_size <<= 1;
        profile = (int *)realloc(profile, profile_size * sizeof(int));
        memset(profile + old_profile_size, 0, (profile_size - old_profile_size) * sizeof(int));
    }
}

void update_profile(int ip) {
    if (profile != NULL) {
        profile[ip]++;
    }
}
