#define _POSIX_C_SOURCE 200809L

#include "signals.h"
#include "profile.h"
#include "terminal.h"

#include <stdbool.h>

bool trigger_save_profile = false;
bool trigger_terminate = false;

#ifdef _WIN32

void init_signals(void) {
}

void handle_signals() {
}

#else // _WIN32

#include <string.h>
#include <signal.h>

void handle_sigusr1(int signal) {
    trigger_save_profile = true;
}

void handle_sigint(int signal) {
    trigger_save_profile = true;
    trigger_terminate = true;
}

void init_signals(void) {
    struct sigaction act = {};
    act.sa_handler = &handle_sigusr1;
    sigaction(SIGUSR1, &act, NULL);
    act.sa_handler = &handle_sigint;
    sigaction(SIGINT, &act, NULL);
}

void handle_signals() {
    if (trigger_save_profile) {
        trigger_save_profile = false;
        save_profile();
    }

    if (trigger_terminate) {
        trigger_terminate = false;

        restore_terminal();

        struct sigaction act = {};
        act.sa_handler = SIG_DFL;
        sigaction(SIGINT, &act, NULL);

        raise(SIGINT);
    }
}

#endif // _WIN32
