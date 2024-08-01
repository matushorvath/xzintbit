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

#ifndef _WIN32

bool trigger_save_profile = false;
bool trigger_interrupt = false;

void handle_sigusr1(int signal) {
    trigger_save_profile = true;
}

void handle_sigint(int signal) {
    trigger_save_profile = true;
    trigger_interrupt = true;
}

#endif // _WIN32

void init_profile(void) {
    profile_size = 64;
    profile = (int *)malloc(profile_size * sizeof(int));
    memset(profile, 0, profile_size * sizeof(int));

#ifndef _WIN32
    struct sigaction act = {};
    act.sa_handler = &handle_sigusr1;
    sigaction(SIGUSR1, &act, NULL);
    act.sa_handler = &handle_sigint;
    sigaction(SIGINT, &act, NULL);
#endif // _WIN32
}

char *get_profile_name(char *program_name) {
    static char profile_name[256];

    time_t t = time(NULL);
    struct tm tm = {};
    localtime_r(&t, &tm);

    sprintf(profile_name, "%s.%02i%02i%02i.%02i%02i%02i.profile.yaml",
        program_name, tm.tm_year - 100, tm.tm_mon + 1, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec);

    return profile_name;
}

void save_profile(char *program_name) {
    if (profile != NULL) {
        FILE *fprofile = fopen(get_profile_name(program_name), "wt");

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

void update_profile(int ip, char *program_name) {
    if (profile != NULL) {
        profile[ip]++;

#ifndef _WIN32
        if (trigger_save_profile) {
            trigger_save_profile = false;
            save_profile(program_name);
        }

        if (trigger_interrupt) {
            trigger_interrupt = false;

            struct sigaction act = {};
            act.sa_handler = SIG_DFL;
            sigaction(SIGINT, &act, NULL);

            raise(SIGINT);
        }
#endif // _WIN32
    }
}
