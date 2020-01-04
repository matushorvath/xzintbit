import * as os from 'os';

interface Op {
    oc: number;
    mds: number[];
};

interface AsmParam {
    ind: boolean,
    rb: boolean,
    val: number,
    sym: string
};

type Frame = { [sym: string]: number };
type FixupData = { sym: string, a: number };

export type Mem = number[];

export class Vm {
    constructor(public id: number, private mem: Mem) {
        this.ip = 0;
        this.rb = 0;
    }

    public ip: number;
    private rb: number;

    dumpMem = (): Mem => {
        return { ...this.mem };
    };

    static cmpMem = (m1: Mem, m2: Mem) => {
        const addrs = new Set([...Object.keys(m1), ...Object.keys(m2)].map(a => Number(a)));
        return new Array(...addrs).filter(a => m1[a] !== m2[a]).map(a => ({ a, m1: m1[a], m2: m2[a] }));
    };

    private getMem = (addr: number) => {
        return this.mem[addr] || 0;
    };

    private setMem = (addr: number, val: number) => {
        this.mem[addr] = val;
    };

    private getOp = () => {
        const ocn = Number(this.getMem(this.ip));
        const oc = ocn % 100;
        const mds = [100, 1000, 10000].map(x => Math.trunc(ocn / x) % 10);

        return { oc, mds };
    };

    private getParam = (o: Op, idx: number) => {
        switch (o.mds[idx]) {
            case 0: { // position mode
                const addr = this.getMem(this.ip + idx + 1);
                const val = this.getMem(addr);
                //console.log(this.id, `G [${addr}] -> ${val}`);
                return val;
            }
            case 1: { // immediate mode
                return this.getMem(this.ip + idx + 1);
            }
            case 2: { // relative mode
                const raddr = this.getMem(this.ip + idx + 1);
                const addr = this.rb + raddr;
                const val = this.getMem(addr);
                //console.log(this.id, `G [rb + ${raddr} = ${addr}] -> ${val}`);
                return val;
            }
            default:
                throw new Error(`mode error: mem ${this.mem} ip ${this.ip} o ${o} idx ${idx}`);
        }
    };

    private setParam = (o: Op, idx: number, val: number) => {
        switch (o.mds[idx]) {
            case 0: { // position mode
                const addr = this.getMem(this.ip + idx + 1);
                this.setMem(addr, val);
                //console.log(this.id, `S [${addr}] <- ${val}`);
                break;
            }
            case 2: { // relative mode
                const raddr = this.getMem(this.ip + idx + 1);
                const addr = this.rb + raddr;
                this.setMem(addr, val);
                //console.log(this.id, `S [rb + ${raddr} = ${addr}] <- ${val}`);
                break;
            }
            default:
                throw new Error(`mode error: mem ${this.mem} ip ${this.ip} o ${o} idx ${idx}`);
        }
    };

    run = async function* (ins: AsyncGenerator<number> = (async function* () {})()): AsyncGenerator<number> {
        while (true) {
            const o = this.getOp();
            //console.log(this.id, 'op', this.ip, this.dasmOp(o).asm.replace('\t', ' '), `rb: ${this.rb}`);

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
                        //console.log(this.id, 'ins done');
                        return;
                    }
                    //console.log(this.id, 'in', value);
                    this.setParam(o, 0, value);
                    this.ip += 2;
                    break;
                }
                case 4: { // out
                    const value = this.getParam(o, 0);
                    //console.log(this.id, 'out', value);
                    this.ip += 2;
                    yield value;
                    break;
                }
                case 5: // jnz
                    if (this.getParam(o, 0) !== 0) {
                        this.ip = this.getParam(o, 1)
                        //console.log(this.id, 'jump', this.ip);
                    } else {
                        this.ip += 3;
                    }
                    break;
                case 6: // jz
                    if (this.getParam(o, 0) === 0) {
                        this.ip = this.getParam(o, 1)
                        //console.log(this.id, 'jump', this.ip);
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
                    //console.log(this.id, `rb <- ${this.rb}`);
                    break;
                case 99: // hlt
                    //console.log(this.id, 'halt');
                    return;
                default:
                    throw new Error(`opcode error: mem ${this.mem} ip ${this.ip} oc ${o.oc}`);
            }

            // console.log(this.ip);
            // console.log(JSON.stringify(this.mem));
        }
    };

    private dasmParam = (o: Op, idx: number) => {
        const val = this.getMem(this.ip + idx + 1);
        switch (o.mds[idx]) {
            case 0: // position mode
                return `[${val}]`;
            case 1: // immediate mode
                return `${val}`;
            case 2: // relative mode
                return `[rb + ${val}]`;
            default:
                return `<unknown>`;
        }
    };

    private dasmOp = (o: Op) => {
        switch (o.oc) {
            case 1:
                return { asm:`add\t${this.dasmParam(o, 0)}, ${this.dasmParam(o, 1)}, ${this.dasmParam(o, 2)}`, ip: 4 };
            case 2: // mul
                return { asm:`mul\t${this.dasmParam(o, 0)}, ${this.dasmParam(o, 1)}, ${this.dasmParam(o, 2)}`, ip: 4 };
            case 3: // in
                return { asm:`in\t${this.dasmParam(o, 0)}`, ip: 2 };
            case 4: // out
                return { asm:`out\t${this.dasmParam(o, 0)}`, ip: 2 };
            case 5: // jnz
                return { asm:`jnz\t${this.dasmParam(o, 0)}, ${this.dasmParam(o, 2)}`, ip: 3 };
            case 6: // jz
                return { asm:`jz\t${this.dasmParam(o, 0)}, ${this.dasmParam(o, 2)}`, ip: 3 };
            case 7: // lt
                return { asm:`lt\t${this.dasmParam(o, 0)}, ${this.dasmParam(o, 1)}, ${this.dasmParam(o, 2)}`, ip: 4 };
            case 8: // eq
                return { asm:`eq\t${this.dasmParam(o, 0)}, ${this.dasmParam(o, 1)}, ${this.dasmParam(o, 2)}`, ip: 4 };
            case 9: // arb
                return { asm:`arb\t${this.dasmParam(o, 0)}`, ip: 2 };
            case 99: // hlt
                return { asm:`hlt`, ip: 1 };
            default:
                return { asm:`db\t${this.getMem(this.ip)}`, ip: 1 };
        }
    };

    dasm = (start: number = 0, len: number = undefined) => {
        const code:{ [ip: string]: string } = {};

        let ipFrom = start || 0;

        let ipTo: number;
        if (len !== undefined) {
            ipTo = start + len;
        } else {
            ipTo = Object.keys(this.mem).reduce((max, val) => max > Number(val) ? max : Number(val), 0);
        }

        this.ip = ipFrom;
        while (this.ip < ipTo) {
            const o = this.getOp();
            const { asm, ip } = this.dasmOp(o);
            code[`${this.ip}`] = asm;
            this.ip += ip;
        }

        return Object.keys(code).map(ip => `${ip}\t${code[Number(ip)]}`).join(os.EOL);
    };

    private parseParam = (lineno: number, idx: number, frame: Frame, param: string) => {
        const m = param.match(/(\[)?(?:rb ([-+]) )?([a-zA-Z_]\w+)?(?: ([-+]) )?((-?[0-9]+)|'(.)')?(\])?/);
        if (!m) {
            throw new Error(`no param match, line ${lineno}, op ${idx}: ${param}`);
        }

        let [_, obr, rbpm, sym, sympm, vc, vals, chr, cbr] = m;
        if (!!obr !== !!cbr) {
            throw new Error(`invalid param, brace mismatch, line ${lineno}, op ${idx}: ${param}`);
        }
        if (!obr && rbpm) {
            throw new Error(`invalid param, no braces and rb, line ${lineno}, op ${idx}: ${param}`);
        }
        if ((!sym && sympm) || (!vals && sympm)) {
            throw new Error(`invalid param, extra plus/minus sign, line ${lineno}, op ${idx}: ${param}`);
        }
        if (!sym && !vc) {
            throw new Error(`invalid param, neither symbol nor value, line ${lineno}, op ${idx}: ${param}`);
        }
        if (rbpm && rbpm === '-' && sym) {
            throw new Error(`invalid param, negative frame symbol, line ${lineno}, op ${idx}: ${param}`);
        }

        const valn = vals ? (rbpm === '-' ? -1 : 1) * (sympm === '-' ? -1 : 1) * Number(vals) : undefined;
        let val = Number(valn ?? chr?.charCodeAt(0) ?? 0);

        // Process frame symbols
        if (sym) {
            if (frame && frame[sym] !== undefined) {
                if (obr && !rbpm) {
                    throw new Error(`invalid param, frame symbol ${sym} used as global pointer, line ${lineno}, op ${idx}: ${param}`);
                }
                val += frame[sym];
                sym = undefined;
            } else if (rbpm) {
                throw new Error(`invalid param, rb + non-frame symbol ${sym}, line ${lineno}, op ${idx}: ${param}`);
            }
        }

        return {
            ind: obr !== undefined,
            rb: rbpm !== undefined,
            val,
            sym
        };
    };

    private asmParam = (ps: AsmParam[], idx: number) => {
        const p = ps[idx];

        return {
            oc: this.oc(p, idx),
            mem: p.val,
            fixups: this.fixup(p, idx + 1)
        }
    };

    private oc = (p: AsmParam, idx: number) => {
        const coef = [100, 1000, 10000];
        const mul = p.ind ? (p.rb ? 2 : 0) : 1;
        return coef[idx] * mul;
    };

    private fixup = (p: AsmParam, offset: number): FixupData[] => {
        return p.sym ? [{ sym: p.sym, a: this.ip + offset }] : []
    };

    private asmOp = (op: string, ps: AsmParam[]) => {
        switch (op) {
            case 'add': {
                if (ps.length !== 3 || !ps[2].ind) {
                    throw new Error('invalid params');
                }
                const aps = [this.asmParam(ps, 0), this.asmParam(ps, 1), this.asmParam(ps, 2)];
                return {
                    mem: [1 + aps[0].oc + aps[1].oc + aps[2].oc, aps[0].mem, aps[1].mem, aps[2].mem],
                    fixups: [...aps[0].fixups, ...aps[1].fixups, ...aps[2].fixups]
                };
            }
            case 'mul': {
                if (ps.length !== 3 || !ps[2].ind) {
                    throw new Error('invalid params');
                }
                const aps = [this.asmParam(ps, 0), this.asmParam(ps, 1), this.asmParam(ps, 2)];
                return {
                    mem: [2 + aps[0].oc + aps[1].oc + aps[2].oc, aps[0].mem, aps[1].mem, aps[2].mem],
                    fixups: [...aps[0].fixups, ...aps[1].fixups, ...aps[2].fixups]
                };
            }
            case 'in': {
                if (ps.length !== 1 || !ps[0].ind) {
                    throw new Error('invalid params');
                }
                const aps = [this.asmParam(ps, 0)];
                return {
                    mem: [3 + aps[0].oc, aps[0].mem],
                    fixups: aps[0].fixups
                };
            }
            case 'out': {
                if (ps.length !== 1) {
                    throw new Error('invalid params');
                }
                const aps = [this.asmParam(ps, 0)];
                return {
                    mem: [4 + aps[0].oc, aps[0].mem],
                    fixups: aps[0].fixups
                };
            }
            case 'jnz': {
                if (ps.length !== 2) {
                    throw new Error('invalid params');
                }
                const aps = [this.asmParam(ps, 0), this.asmParam(ps, 1)];
                return {
                    mem: [5 + aps[0].oc + aps[1].oc, aps[0].mem, aps[1].mem],
                    fixups: [...aps[0].fixups, ...aps[1].fixups]
                };
            }
            case 'jz': {
                if (ps.length !== 2) {
                    throw new Error('invalid params');
                }
                const aps = [this.asmParam(ps, 0), this.asmParam(ps, 1)];
                return {
                    mem: [6 + aps[0].oc + aps[1].oc, aps[0].mem, aps[1].mem],
                    fixups: [...aps[0].fixups, ...aps[1].fixups]
                };
            }
            case 'lt': {
                if (ps.length !== 3 || !ps[2].ind) {
                    throw new Error('invalid params');
                }
                const aps = [this.asmParam(ps, 0), this.asmParam(ps, 1), this.asmParam(ps, 2)];
                return {
                    mem: [7 + aps[0].oc + aps[1].oc + aps[2].oc, aps[0].mem, aps[1].mem, aps[2].mem],
                    fixups: [...aps[0].fixups, ...aps[1].fixups, ...aps[2].fixups]
                };
            }
            case 'eq': {
                if (ps.length !== 3 || !ps[2].ind) {
                    throw new Error('invalid params');
                }
                const aps = [this.asmParam(ps, 0), this.asmParam(ps, 1), this.asmParam(ps, 2)];
                return {
                    mem: [8 + aps[0].oc + aps[1].oc + aps[2].oc, aps[0].mem, aps[1].mem, aps[2].mem],
                    fixups: [...aps[0].fixups, ...aps[1].fixups, ...aps[2].fixups]
                };
            }
            case 'arb': {
                if (ps.length !== 1) {
                    throw new Error('invalid params');
                }
                const aps = [this.asmParam(ps, 0)];
                return {
                    mem: [9 + aps[0].oc, aps[0].mem],
                    fixups: aps[0].fixups
                };
            }
            case 'hlt': {
                if (ps.length !== 0) {
                    throw new Error('invalid params');
                }
                return {
                    mem: [99],
                    fixups: [] as FixupData[]
                };
            }
            case 'db': {
                if (ps.length !== 1 || ps[0].ind || ps[0].rb) {
                    throw new Error('invalid params');
                }
                return {
                    mem: [ps[0].val],
                    fixups: this.fixup(ps[0], 0)
                };
            }
            case 'ds': {
                if (ps.length !== 2 || ps[0].ind || ps[0].rb || ps[1].ind || ps[1].rb) {
                    throw new Error('invalid params');
                }
                return {
                    mem: Array.from({ length: ps[0].val }).fill(ps[1].val) as number[],
                    fixups: [] as FixupData[] // TODO
                };
            }
            case 'cal': {
                if (ps.length !== 1) {
                    throw new Error('invalid params');
                }
                return {
                    mem: [
                        109, -1,                                    // arb -1
                        21101, this.ip + 9, 0, 0,                   // add ip + 2 + 4 + 3, 0, [rb + 0]
                        106 + this.oc(ps[0], 1), 0, ps[0].val       // jz 0, $0
                    ],
                    fixups: this.fixup(ps[0], 8)
                };
            }
            case 'ret': {
                if (ps.length !== 1 || ps[0].ind || ps[0].rb) {
                    throw new Error('invalid params');
                }
                return {
                    mem: [
                        109, ps[0].val + 1,                         // arb $0 + 1
                        2106, 0, -(ps[0].val + 1)                   // jz 0, [rb - ($0 + 1)]
                    ],
                    fixups: this.fixup(ps[0], 1)
                };
            }
            default:
                throw new Error('invalid opcode');
        }
    };

    asm = (code: string) => {
        const lines = code.split(/\r?\n/);

        const fixups: { [sym: string]: { a: number, ln: number }[] } = {};
        const syms: { [sym: string]: number } = {};
        let frame: Frame;

        this.ip = 0;
        let lineno = 1;
        while (true) {
            if (lineno - 1 >= lines.length) {
                throw new Error('missing .EOF at the end of file');
            }
            const line = lines[lineno - 1];

            const lm = line.match(/^(?:\+([0123]) = )?(\w+):(?:\s*#.*)?|^(?:\.([A-Z]+))(?:\s+([^#]+))?(?:#.*)?|^\s+([a-z]+)(?:\s+([^#]+))?(?:\s*#.*)?|^\s*#|^\s*$/);
            if (!lm) {
                throw new Error(`no line match, line ${lineno}: ${line}`);
            }

            const [_, inc, sym, dir, dpsss, op, opss] = lm;

            if (sym !== undefined) {
                if (sym in syms) {
                    throw new Error(`duplicate global symbol ${sym}, line ${lineno}: ${line}`);
                }
                syms[sym] = this.ip + Number(inc ?? 0);
            } else if (dir !== undefined) {
                if (dir === 'FRAME') {
                    if (frame) {
                        throw new Error(`.FRAME when frame already started, line ${lineno}: ${line}`);
                    }

                    let pss = dpsss === undefined ? [] : dpsss.split('; ').map(ps => ps.split(', '));

                    // We first assume last component is missing, then first component is missing, then all are missing
                    if (pss.length > 3) {
                        throw new Error(`.FRAME with more than two semicolons, line ${lineno}: ${line}`);
                    } else if (pss.length === 2) {
                        pss = [...pss, []];
                    } else if (pss.length === 1) {
                        pss = [[], ...pss, []];
                    } else if (pss.length === 0) {
                        pss = [[], [], []];
                    }

                    //console.log(pss);

                    // Local variables below rb
                    const nv = pss[2].map((s, i) => ({ s, o: -i - 1 }));
                    // Local variables
                    const lv = pss[1].reverse().map((s, i) => ({ s, o: i }));
                    // Function parameters
                    const ps = pss[0].reverse().map((s, i) => ({ s, o: lv.length + i + 1 }));

                    //console.log(nv, lv, ps);

                    frame = {};
                    for (const { s, o } of [...nv, ...lv, ...ps]) {
                        if (s in frame) {
                            throw new Error(`duplicate frame symbol ${sym}, line ${lineno}: ${line}`);
                        }
                        frame[s] = o;
                    }
                    //console.log('f', frame);
                } else if (dir === 'ENDFRAME') {
                    if (dpsss) {
                        throw new Error(`.ENDFRAME with params, line ${lineno}: ${line}`);
                    }
                    if (!frame) {
                        throw new Error(`.ENDFRAME when no frame started, line ${lineno}: ${line}`);
                    }
                    frame = undefined;
                    //console.log('f', frame);
                } else if (dir === 'EOF') {
                    // Explicit end of file, needed for intcode implementation to detect EOF
                    break;
                }
            } else if (op !== undefined) {
                const ps = opss === undefined ? [] : opss.split(', ')
                    .map((p, i) => this.parseParam(lineno, i, frame, p));

                try {
                    //console.log('i', op, ps);
                    const { mem, fixups: addFixups } = this.asmOp(op, ps);
                    //console.log('a', mem, addFixups);

                    for (const fixup of addFixups) {
                        if (fixups[fixup.sym] === undefined) {
                            fixups[fixup.sym] = [{ a: fixup.a, ln: lineno }]
                        } else {
                            fixups[fixup.sym].push({ a: fixup.a, ln: lineno });
                        }
                    }
                    //console.log('f', fixups);

                    this.mem.push(...mem);
                    this.ip += mem.length;
                } catch (error) {
                    throw new Error(`${error.message}, line ${lineno}: ${line}`);
                }
            }
            lineno += 1;
        }

        for (const sym of Object.keys(fixups)) {
            const fixup = fixups[sym];
            if (syms[sym] === undefined) {
                throw new Error(`unknown global symbol ${sym}, line ${fixup[0].ln}`);
            }
            const val = syms[sym];
            for (const { a: addr } of fixup) {
                this.mem[addr] += val;
            }
        }
    };

    getAsInput = () => {
        return this.mem.join(',') + '\n';
    };
}
