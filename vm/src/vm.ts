export class Vm {
    constructor(private mem: number[]) {
    }

    private ip = 0;
    private rb = 0;

    private getParam = (idx: number) => {
        const mode = Math.trunc((this.mem[this.ip] ?? 0) / [100, 1000, 10000][idx]) % 10;
        switch (mode) {
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
                throw new Error(`mode error: mem ${this.mem} ip ${this.ip} idx ${idx}`);
        }
    };

    private setParam = (idx: number, val: number) => {
        const mode = Math.trunc((this.mem[this.ip] ?? 0) / [100, 1000, 10000][idx]) % 10;
        switch (mode) {
            case 0: { // position mode
                this.mem[this.mem[this.ip + idx + 1] ?? 0] = val;
                break;
            }
            case 2: { // relative mode
                this.mem[this.rb + this.mem[this.ip + idx + 1] ?? 0] = val;
                break;
            }
            default:
                throw new Error(`mode error: mem ${this.mem} ip ${this.ip} idx ${idx}`);
        }
    };

    run = async function* (ins: AsyncGenerator<number> = (async function* () {})()): AsyncGenerator<number> {
        while (true) {
            const oc = (this.mem[this.ip] ?? 0) % 100;

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
                        this.ip = this.getParam(1)
                    } else {
                        this.ip += 3;
                    }
                    break;
                case 6: // jz
                    if (this.getParam(0) === 0) {
                        this.ip = this.getParam(1)
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
                    throw new Error(`opcode error: mem ${this.mem} ip ${this.ip} oc ${oc}`);
            }
        }
    };
}
