interface Op {
    oc: number;
    mds: number[];
};

export class Vm {
    constructor(public id: number, private mem: number[]) {
        this.ip = 0;
        this.rb = 0;
    }

    public ip: number;
    private rb: number;

    private getOp = () => {
        const val = this.mem[this.ip] ?? 0;
        return {
            oc: val % 100,
            mds: [100, 1000, 10000].map(i => Math.trunc(val / i) % 10)
        }
    };

    private getParam = (o: Op, idx: number) => {
        switch (o.mds[idx]) {
            case 0: { // position mode
                return this.mem[this.mem[this.ip + idx + 1] ?? 0] ?? 0;
            }
            case 1: { // immediate mode
                return this.mem[this.ip + idx + 1] ?? 0;
            }
            case 2: { // relative mode
                return this.mem[this.rb + this.mem[this.ip + idx + 1] ?? 0] ?? 0;
            }
            default:
                throw new Error(`mode error: mem ${this.mem} ip ${this.ip} o ${o} idx ${idx}`);
        }
    };

    private setParam = (o: Op, idx: number, val: number) => {
        switch (o.mds[idx]) {
            case 0: { // position mode
                this.mem[this.mem[this.ip + idx + 1] ?? 0] = val;
                break;
            }
            case 2: { // relative mode
                this.mem[this.rb + this.mem[this.ip + idx + 1] ?? 0] = val;
                break;
            }
            default:
                throw new Error(`mode error: mem ${this.mem} ip ${this.ip} o ${o} idx ${idx}`);
        }
    };

    run = async function* (ins: AsyncGenerator<number> = (async function* () {})()): AsyncGenerator<number> {
        while (true) {
            const o = this.getOp();

            switch (o.oc) {
                case 1: // add
                    this.setParam(o, 2, this.getParam(o, 0) + this.getParam(o, 1));
                    this.ip += 4;
                    break;
                case 2: // mul
                    this.setParam(o, 2, this.getParam(o, 0) * this.getParam(o, 1));
                    this.ip += 4;
                    break;
                case 3: { // in
                    const { value, done } = await ins.next();
                    if (done) {
                        throw new Error('no more inputs');
                    }
                    this.setParam(o, 0, value);
                    this.ip += 2;
                    break;
                }
                case 4: { // out
                    const value = this.getParam(o, 0);
                    this.ip += 2;
                    yield value;
                    break;
                }
                case 5: // jnz
                    if (this.getParam(o, 0) !== 0) {
                        this.ip = this.getParam(o, 1)
                    } else {
                        this.ip += 3;
                    }
                    break;
                case 6: // jz
                    if (this.getParam(o, 0) === 0) {
                        this.ip = this.getParam(o, 1)
                    } else {
                        this.ip += 3;
                    }
                    break;
                case 7: // lt
                    this.setParam(o, 2, this.getParam(o, 0) < this.getParam(o, 1) ? 1 : 0);
                    this.ip += 4;
                    break;
                case 8: // eq
                    this.setParam(o, 2, this.getParam(o, 0) === this.getParam(o, 1) ? 1 : 0);
                    this.ip += 4;
                    break;
                case 9: // arb
                    this.rb += this.getParam(o, 0);
                    this.ip += 2;
                    break;
                case 99: // hlt
                    return;
                default:
                    throw new Error(`opcode error: mem ${this.mem} ip ${this.ip} oc ${o.oc}`);
            }
        }
    };
}
