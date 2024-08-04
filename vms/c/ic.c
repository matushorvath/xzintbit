#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#ifdef _WIN32
#   include <fcntl.h>
#   include <io.h>
#endif // _WIN32

#include "profile.h"

int *mem = NULL;
int mem_size = 0;

int ip = 0;
int rb = 0;

void parse_command_line(int argc, char **argv, char **program_name, bool *enable_profile) {
    *enable_profile = false;
    *program_name = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-p") == 0) {
            *enable_profile = true;
        } else {
            *program_name = argv[i];
        }
    }
}

void resize_mem(int addr) {
    if (addr >= mem_size) {
        int old_mem_size = mem_size;
        while (addr >= mem_size) mem_size <<= 1;
        mem = (int *)realloc(mem, mem_size * sizeof(int));
        memset(mem + old_mem_size, 0, (mem_size - old_mem_size) * sizeof(int));
    }

    resize_profile(addr);
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

        update_profile(ip, program_name);
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
    _setmode(_fileno(stderr), _O_BINARY);
#endif // _WIN32

    char *program_name;
    bool enable_profile;
    parse_command_line(argc, argv, &program_name, &enable_profile);

    mem_size = 64;
    mem = (int*)malloc(mem_size * sizeof(int));
    memset(mem, 0, mem_size * sizeof(int));

    if (enable_profile) {
        init_profile();
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

    save_profile(program_name);

    return 0;
}
