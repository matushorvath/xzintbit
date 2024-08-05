// test standard behavior:
// ICVM=~/intcode/xzintbit/vms/c-ext/ics make test

// test extended behavior:
// ICVM_TYPE=c-ext make test

// test manually with a real console:
//
// ~/intcode/xzintbit/vms/c-ext/ic test/extended_vm_in/bin/extended_vm_in.input
// (press A B C, it should wait forever for next character)
//
// ~/intcode/xzintbit/vms/c-ext/ic test/extended_vm_ina/bin/extended_vm_ina.input
// (press A B C D, should print "fail" after D, not sooner, and exit)

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "profile.h"
#include "signals.h"
#include "terminal.h"

int *mem = NULL;
int mem_size = 0;

int ip = 0;
int rb = 0;

int extended = false;

void parse_command_line(int argc, char **argv, char **program_name, bool *enable_profile, bool *enable_extended) {
    *program_name = NULL;
    *enable_profile = false;
    *enable_extended = true;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-p") == 0) {
            *enable_profile = true;
        } else if (strcmp(argv[i], "-s") == 0) {
            *enable_extended = false;
        } else {
            *program_name = argv[i];
        }
    }
}

void init_extended_vm(void) {
    extended = false;

    // Check whether 'jnz 0, addr' is the first instruction
    if (mem_size < 3 || mem[0] != 1105 || mem[1] != 0) {
        return;
    }

    // Check if the extended starting address is valid
    int address = mem[2];
    if (address < 0 || address >= mem_size) {
        return;
    }

    // Use the extended starting address instead of 0
    extended = true;
    ip = address;
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

int is_feature(int id) {
    return (id >= 1 && id <= 9) || id == 99 || id == 10 || id == 13;
}

int get_input() {
    return read_sync();
}

int get_input_async() {
    return read_async();
}

void set_output(int val) {
    putc(val, stdout);
    fflush(stdout);
}

int run() {
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
                if (value == READ_EOF) {
                    fprintf(stderr, "no more inputs\n");
                    return 1;
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
                return 0;
            case 10: // ftr
                if (extended) {
                    // ftr ftrid, [dst]: set [dst] to 1 if feature ftrid is supported
                    set_param(1, is_feature(get_param(0)) ? 1 : 0);
                    ip += 3;
                    break;
                }
            case 13: // ina
                if (extended) {
                    // ina [dst]: asynchronous input, read a character if available, otherwise set [dst] to -1
                    int value = get_input_async();
                    if (value == READ_EOF) {
                        fprintf(stderr, "no more inputs\n");
                        return 1;
                    }
                    set_param(0, value == READ_NO_DATA ? -1 : value);
                    ip += 2;
                    break;
                }
            default:
                fprintf(stderr, "opcode error: ip %d oc %d\n", ip, oc);
                return 1;
        }

        update_profile(ip);
        handle_signals();
    }
}

void load_binary(char *program_name) {
    mem_size = 64;
    mem = (int*)malloc(mem_size * sizeof(int));
    memset(mem, 0, mem_size * sizeof(int));

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
}

int main(int argc, char **argv) {
    char *program_name;
    bool enable_profile, enable_extended;
    parse_command_line(argc, argv, &program_name, &enable_profile, &enable_extended);

    load_binary(program_name);

    if (enable_extended) {
        init_extended_vm();
    }

    init_terminal(extended);
    init_signals();

    if (enable_profile) {
        init_profile(program_name);
    }

    int result = run();

    save_profile();

    return result;
}
