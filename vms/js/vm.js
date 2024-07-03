import { writeStreamAndWait } from './util.js';

export class Vm {
    mem = [];

    ip = 0;
    rb = 0;

    trace = false;

    getMem(addr) {
        return this.mem[addr] ?? 0;
    }

    setMem(addr, val) {
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

    static OPCODES = {
        1: { name: 'add', length: 4 },
        2: { name: 'mul', length: 4 },
        3: { name: 'in', length: 2 },
        4: { name: 'out', length: 2 },
        5: { name: 'jnz', length: 3 },
        6: { name: 'jz', length: 3 },
        7: { name: 'lt', length: 4 },
        8: { name: 'eq', length: 4 },
        9: { name: 'arb', length: 2 },
        99: { name: 'hlt', length: 1 }
    };

    getParamData(idx, value) {
        const mode = Math.floor(this.getMem(this.ip) / Vm.MODE_MUL[idx]) % 10;

        switch (mode) {
        case 0:
            return [this.map?.[value] ? `[${this.map[value]}(${value})]` : `[${value}]`, this.getMem(value)];
        case 1:
            return [this.map?.[value] ? `${this.map[value]}(${value})` : `${value}`, value];
        case 2:
            return [`[rb ${value < 0 ? '-' : '+'} ${Math.abs(value)} = ${this.rb + value}]`, this.getMem(this.rb + value)];
        default:
            return [`${value}`, value];
        }
    }

    async printTrace() {
        const op = this.getMem(this.ip);

        const ipSymbol = this.map?.[this.ip];
        if (ipSymbol !== undefined) {
            await writeStreamAndWait(process.stderr, `${ipSymbol}:\n`);
        }

        const info = Vm.OPCODES[op % 100];

        const addrStr = String(this.ip).padStart(6, ' ');
        const ocStr = `${info?.name ?? '???'}(${op})`;

        const paramData = this.mem.slice(this.ip + 1, this.ip + (info?.length ?? 4))
            .map((value, idx) => this.getParamData(idx, value));

        const paramAddrs = paramData.map(d => d[0]).join(', ');
        const paramVals = paramData.map(d => `${d[1]}(${d[1].toString(16)})`).join(', ');

        const instruction = `${addrStr}: ${ocStr} ${paramAddrs}`;
        const padding = ' '.repeat(Math.max(70 - instruction.length, 5));
        const state = `| params ${paramVals}`;

        await writeStreamAndWait(process.stderr, `${instruction}${padding}${state}\n`);
    }

    async* run(mem, ins = (async function* () {})()) {
        this.ip = this.rb = 0;
        this.mem = mem;

        while (true) {
            if (this.ip === this.traceAddress) {
                this.trace = true;
            }
            if (this.trace) {
                await this.printTrace();
            }

            const oc = Math.floor(this.getMem(this.ip) % 100);

            if (this.debug && oc === -42) {
                debugger;                                   // eslint-disable-line no-debugger
                this.ip += 1;
                continue;
            }

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
        }
    }
}
