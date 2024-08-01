#define _POSIX_C_SOURCE 200809L

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#ifdef _WIN32
#   include <fcntl.h>
#   include <io.h>
#endif

#ifndef WIN32
#   include <signal.h>
#endif

int *mem = NULL;
int mem_size = 0;

int ip = 0;
int rb = 0;

int *profile = NULL;
int profile_size = 0;

bool trigger_save_profile = false;
bool trigger_interrupt = false;

void parse_command_line(int argc, char **argv, char **program_name, bool *option_profile) {
    *option_profile = false;
    *program_name = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-p") == 0) {
            *option_profile = true;
        } else {
            *program_name = argv[i];
        }
    }
}

#ifndef _WIN32

void handle_sigusr1(int signal) {
    trigger_save_profile = true;
}

void handle_sigint(int signal) {
    trigger_save_profile = true;
    trigger_interrupt = true;
}

#endif

void save_profile(char *program_name) {
    char profile_name[256];
    sprintf(profile_name, "%s.profile.yaml", program_name);

    FILE *fprofile = fopen(profile_name, "wt");

    for (int i = 0; i < mem_size; i++) {
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

void resize_mem(int addr) {
    if (addr >= mem_size) {
        int old_mem_size = mem_size;
        while (addr >= mem_size) mem_size <<= 1;
        mem = (int *)realloc(mem, mem_size * sizeof(int));
        memset(mem + old_mem_size, 0, (mem_size - old_mem_size) * sizeof(int));
    }

    if (profile != NULL && addr >= profile_size) {
        int old_profile_size = profile_size;
        while (addr >= profile_size) profile_size <<= 1;
        profile = (int *)realloc(profile, profile_size * sizeof(int));
        memset(profile + old_profile_size, 0, (profile_size - old_profile_size) * sizeof(int));
    }
}

int get_mem(int addr) {
    resize_mem(addr);
    return mem[addr];
}

void set_mem(int addr, int val) {
    resize_mem(addr);
    mem[addr] = val;
}

const int MODE_MUL[] = { 100, 1000, 10000 };

int get_param(int idx) {
    int mode = get_mem(ip) / MODE_MUL[idx] % 10;
    switch (mode) {
        case 0: // position mode
            return get_mem(get_mem(ip + idx + 1));
        case 1: // immediate mode
            return get_mem(ip + idx + 1);
        case 2: // relative mode
            return get_mem(rb + get_mem(ip + idx + 1));
        default:
            fprintf(stderr, "mode error: ip %d idx %d\n", ip, idx);
            exit(1);
    }
}

void set_param(int idx, int val) {
    int mode = get_mem(ip) / MODE_MUL[idx] % 10;
    switch (mode) {
        case 0: // position mode
            set_mem(get_mem(ip + idx + 1), val);
            break;
        case 2: // relative mode
            set_mem(rb + get_mem(ip + idx + 1), val);
            break;
        default:
            fprintf(stderr, "mode error: ip %d idx %d\n", ip, idx);
            exit(1);
    }
}

void run(char *program_name, int (*get_input)(), void (*set_output)(int)) {
    while (true) {
        int oc = get_mem(ip) % 100;

        switch (oc) {
            case 1: // add
                set_param(2, get_param(0) + get_param(1));
                ip += 4;
                break;
            case 2: // mul
                set_param(2, get_param(0) * get_param(1));
                ip += 4;
                break;
            case 3: { // in
                int value = get_input();
                if (value == EOF) {
                    fprintf(stderr, "no more inputs\n");
                    exit(1);
                }
                set_param(0, value);
                ip += 2;
                break;
            }
            case 4: { // out
                int value = get_param(0);
                ip += 2;
                set_output(value);
                break;
            }
            case 5: // jnz
                if (get_param(0) != 0) {
                    ip = get_param(1);
                } else {
                    ip += 3;
                }
                break;
            case 6: // jz
                if (get_param(0) == 0) {
                    ip = get_param(1);
                } else {
                    ip += 3;
                }
                break;
            case 7: // lt
                set_param(2, get_param(0) < get_param(1) ? 1 : 0);
                ip += 4;
                break;
            case 8: // eq
                set_param(2, get_param(0) == get_param(1) ? 1 : 0);
                ip += 4;
                break;
            case 9: // arb
                rb += get_param(0);
                ip += 2;
                break;
            case 99: // hlt
                return;
            default:
                fprintf(stderr, "opcode error: ip %d oc %d\n", ip, oc);
                exit(1);
        }

        if (profile != NULL) {
            profile[ip]++;

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
        }
    }
}

int get_input() {
    return getc(stdin);
}

void set_output(int val) {
    putc(val, stdout);
    fflush(stdout);
}

int main(int argc, char **argv) {
#ifdef _WIN32
    _setmode(_fileno(stdin), _O_BINARY);
    _setmode(_fileno(stdout), _O_BINARY);
#endif

#ifndef _WIN32
    struct sigaction act = {};
    act.sa_handler = &handle_sigusr1;
    sigaction(SIGUSR1, &act, NULL);
    act.sa_handler = &handle_sigint;
    sigaction(SIGINT, &act, NULL);
#endif

    mem_size = 64;
    mem = (int*)malloc(mem_size * sizeof(int));
    memset(mem, 0, mem_size * sizeof(int));

    char *program_name;
    bool option_profile;
    parse_command_line(argc, argv, &program_name, &option_profile);

    if (option_profile) {
        profile_size = 64;
        profile = (int *)malloc(profile_size * sizeof(int));
        memset(profile, 0, profile_size * sizeof(int));
    }

    FILE *input = fopen(program_name, "rt");

    int idx = 0;
    char ch = ',';
    int num = 0;

    while (ch == ',') {
        if (fscanf(input, "%d%c", &num, &ch) != 2) {
            fprintf(stderr, "parse error");
            exit(1);
        }
        set_mem(idx++, num);
    }

    run(program_name, get_input, set_output);

    if (profile != NULL) {
        save_profile(program_name);
    }

    return 0;
}
