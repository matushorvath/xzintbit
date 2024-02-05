/* eslint-env node */

import timers from 'node:timers/promises';

export class Vm {
    mem = [];

    ip = 0;
    rb = 0;

    cycle = 0;

    reads = {};
    writes = {};

    buildUpdate() {
        const update = {
            ip: this.ip,
            rb: this.rb,
            cycle: this.cycle,
            size: this.mem.length,
            reads: this.reads,
            writes: this.writes
        };

        this.reads = {};
        this.writes = {};

        return update;
    }

    getMem(addr) {
        this.reads[addr] = this.cycle;
        return this.mem[addr] ?? 0;
    }

    setMem(addr, val) {
        this.writes[addr] = this.cycle;
        this.mem[addr] = val;
    }

    static MODE_MUL = [100, 1000, 10000];

    getParam(idx) {
        const mode = Math.floor(this.getMem(this.ip) / Vm.MODE_MUL[idx]) % 10;
        switch (mode) {
            case 0: // position mode
                return this.getMem(this.getMem(this.ip + idx + 1));
            case 1: // immediate mode
                return this.getMem(this.ip + idx + 1);
            case 2: // relative mode
                return this.getMem(this.rb + this.getMem(this.ip + idx + 1));
            default:
                throw new Error(`mode error: ip ${this.ip} idx ${idx}`);
        }
    }

    setParam(idx, val) {
        const mode = Math.floor(this.getMem(this.ip) / Vm.MODE_MUL[idx]) % 10;
        switch (mode) {
            case 0: // position mode
                this.setMem(this.getMem(this.ip + idx + 1), val);
                break;
            case 2: // relative mode
                this.setMem(this.rb + this.getMem(this.ip + idx + 1), val);
                break;
            default:
                throw new Error(`mode error: ip ${this.ip} idx ${idx}`);
        }
    }

    async* run(mem, ins = (async function* () {})()) {
        this.ip = this.rb = this.cycle = 0;
        this.mem = mem;
        this.reads = {};
        this.writes = {};

        while (true) {
            const oc = Math.floor(this.getMem(this.ip) % 100);

            switch (oc) {
                case 1: // add
                    this.setParam(2, this.getParam(0) + this.getParam(1));
                    this.ip += 4;
                    break;
                case 2: // mul
                    this.setParam(2, this.getParam(0) * this.getParam(1));
                    this.ip += 4;
                    break;
                case 3: { // in
                    const { value, done } = await ins.next();
                    if (done) {
                        throw new Error('no more inputs');
                    }
                    this.setParam(0, value);
                    this.ip += 2;
                    break;
                }
                case 4: { // out
                    const value = this.getParam(0);
                    this.ip += 2;
                    yield value;
                    break;
                }
                case 5: // jnz
                    if (this.getParam(0) !== 0) {
                        this.ip = this.getParam(1);
                    } else {
                        this.ip += 3;
                    }
                    break;
                case 6: // jz
                    if (this.getParam(0) === 0) {
                        this.ip = this.getParam(1);
                    } else {
                        this.ip += 3;
                    }
                    break;
                case 7: // lt
                    this.setParam(2, this.getParam(0) < this.getParam(1) ? 1 : 0);
                    this.ip += 4;
                    break;
                case 8: // eq
                    this.setParam(2, this.getParam(0) === this.getParam(1) ? 1 : 0);
                    this.ip += 4;
                    break;
                case 9: // arb
                    this.rb += this.getParam(0);
                    this.ip += 2;
                    break;
                case 99: // hlt
                    return;
                default:
                    throw new Error(`opcode error: ip ${this.ip} oc ${oc}`);
            }

            this.cycle++;

            // Yield every N instructions, to allow a GUI update
            // TODO yield based on a FPS value, and count number of cycles since last in/out
            if (this.cycle % 100 === 0) {
                await timers.scheduler.yield();
            }
        }
    }
};
