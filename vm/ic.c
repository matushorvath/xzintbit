#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <string.h>

int64_t *mem = NULL;
int64_t mem_size = 0;

int64_t ip = 0;
int64_t rb = 0;

void resize_mem(int64_t addr) {
    if (addr >= mem_size) {
        int64_t old_mem_size = mem_size;
        while (addr >= mem_size) mem_size <<= 1;
        mem = realloc(mem, mem_size * sizeof(int64_t));
        memset(mem + old_mem_size, 0, (mem_size - old_mem_size) * sizeof(int64_t));
    }
}

int64_t get_mem(int64_t addr) {
    resize_mem(addr);
    return mem[addr];
}

int64_t set_mem(int64_t addr, int64_t val) {
    resize_mem(addr);
    mem[addr] = val;
}

const int MODE_MUL[] = { 100, 1000, 10000 };

int64_t get_param(int idx) {
    int mode = get_mem(ip) / MODE_MUL[idx] % 10;
    switch (mode) {
        case 0: // position mode
            return get_mem(get_mem(ip + idx + 1));
        case 1: // immediate mode
            return get_mem(ip + idx + 1);
        case 2: // relative mode
            return get_mem(rb + get_mem(ip + idx + 1));
        default:
            fprintf(stderr, "mode error: ip %" PRId64 " idx %d\n", ip, idx);
            exit(1);
    }
}

int64_t set_param(int idx, int64_t val) {
    int mode = get_mem(ip) / MODE_MUL[idx] % 10;
    switch (mode) {
        case 0: // position mode
            set_mem(get_mem(ip + idx + 1), val);
            break;
        case 2: // relative mode
            set_mem(rb + get_mem(ip + idx + 1), val);
            break;
        default:
            fprintf(stderr, "mode error: ip %" PRId64 " idx %d\n", ip, idx);
            exit(1);
    }
}

void run(int64_t (*get_input)(), void (*set_output)(int64_t)) {
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
                int64_t value = get_input();
                if (value == EOF) {
                    fprintf(stderr, "no more inputs\n");
                    exit(1);
                }
                set_param(0, value);
                ip += 2;
                break;
            }
            case 4: { // out
                int64_t value = get_param(0);
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
                fprintf(stderr, "opcode error: ip %" PRId64 " oc %d\n", ip, oc);
                exit(1);
        }
    }
}

int64_t get_input() {
    return getc(stdin);
}

void set_output(int64_t val) {
    putc(val, stdout);
}

int main(int argc, char **argv) {
    mem_size = 64;
    mem = (int64_t*)malloc(mem_size * sizeof(int64_t));
    memset(mem, 0, mem_size * sizeof(int64_t));

    FILE *input = fopen(argv[1], "rt");

    int idx = 0;
    char ch = ',';
    int64_t num = 0;

    while (ch == ',') {
        fscanf(input, "%" SCNd64 "%c", &num, &ch);
        set_mem(idx++, num);
    }

    run(get_input, set_output);
}
