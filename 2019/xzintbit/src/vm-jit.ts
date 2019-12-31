interface Op {
    oc: number;
    mds: number[];
};

type Record = [Function, number, Action?];
export type Mem = { n: number, r?: Record }[];

interface State {
    ip: number;
    rb: number;

    gpos: (i: number) => number;
    gimm: (i: number) => number;
    grel: (i: number) => number;
    spos: (i: number, v: number) => void;
    srel: (i: number, v: number) => void;
}

enum Action { In, Out, Hlt }

const ops: { [oc: number]: Record } = {
        1: [(s: State) => s.spos(3, s.gpos(1) + s.gpos(2)), 4],
      101: [(s: State) => s.spos(3, s.gimm(1) + s.gpos(2)), 4],
      201: [(s: State) => s.spos(3, s.grel(1) + s.gpos(2)), 4],
     1001: [(s: State) => s.spos(3, s.gpos(1) + s.gimm(2)), 4],
     1101: [(s: State) => s.spos(3, s.gimm(1) + s.gimm(2)), 4],
     1201: [(s: State) => s.spos(3, s.grel(1) + s.gimm(2)), 4],
     2001: [(s: State) => s.spos(3, s.gpos(1) + s.grel(2)), 4],
     2101: [(s: State) => s.spos(3, s.gimm(1) + s.grel(2)), 4],
     2201: [(s: State) => s.spos(3, s.grel(1) + s.grel(2)), 4],
    20001: [(s: State) => s.srel(3, s.gpos(1) + s.gpos(2)), 4],
    20101: [(s: State) => s.srel(3, s.gimm(1) + s.gpos(2)), 4],
    20201: [(s: State) => s.srel(3, s.grel(1) + s.gpos(2)), 4],
    21001: [(s: State) => s.srel(3, s.gpos(1) + s.gimm(2)), 4],
    21101: [(s: State) => s.srel(3, s.gimm(1) + s.gimm(2)), 4],
    21201: [(s: State) => s.srel(3, s.grel(1) + s.gimm(2)), 4],
    22001: [(s: State) => s.srel(3, s.gpos(1) + s.grel(2)), 4],
    22101: [(s: State) => s.srel(3, s.gimm(1) + s.grel(2)), 4],
    22201: [(s: State) => s.srel(3, s.grel(1) + s.grel(2)), 4],

        2: [(s: State) => s.spos(3, s.gpos(1) * s.gpos(2)), 4],
      102: [(s: State) => s.spos(3, s.gimm(1) * s.gpos(2)), 4],
      202: [(s: State) => s.spos(3, s.grel(1) * s.gpos(2)), 4],
     1002: [(s: State) => s.spos(3, s.gpos(1) * s.gimm(2)), 4],
     1102: [(s: State) => s.spos(3, s.gimm(1) * s.gimm(2)), 4],
     1202: [(s: State) => s.spos(3, s.grel(1) * s.gimm(2)), 4],
     2002: [(s: State) => s.spos(3, s.gpos(1) * s.grel(2)), 4],
     2102: [(s: State) => s.spos(3, s.gimm(1) * s.grel(2)), 4],
     2202: [(s: State) => s.spos(3, s.grel(1) * s.grel(2)), 4],
    20002: [(s: State) => s.srel(3, s.gpos(1) * s.gpos(2)), 4],
    20102: [(s: State) => s.srel(3, s.gimm(1) * s.gpos(2)), 4],
    20202: [(s: State) => s.srel(3, s.grel(1) * s.gpos(2)), 4],
    21002: [(s: State) => s.srel(3, s.gpos(1) * s.gimm(2)), 4],
    21102: [(s: State) => s.srel(3, s.gimm(1) * s.gimm(2)), 4],
    21202: [(s: State) => s.srel(3, s.grel(1) * s.gimm(2)), 4],
    22002: [(s: State) => s.srel(3, s.gpos(1) * s.grel(2)), 4],
    22102: [(s: State) => s.srel(3, s.gimm(1) * s.grel(2)), 4],
    22202: [(s: State) => s.srel(3, s.grel(1) * s.grel(2)), 4],

        3: [(s: State, i: number) => s.spos(1, i), 2, Action.In],
      203: [(s: State, i: number) => s.srel(1, i), 2, Action.In],

        4: [(s: State) => s.gpos(1), 2, Action.Out],
      104: [(s: State) => s.gimm(1), 2, Action.Out],
      204: [(s: State) => s.grel(1), 2, Action.Out],

        5: [(s: State) => s.ip = s.gpos(1) !== 0 ? s.gpos(2) : s.ip + 3, 0],
      105: [(s: State) => s.ip = s.gimm(1) !== 0 ? s.gpos(2) : s.ip + 3, 0],
      205: [(s: State) => s.ip = s.grel(1) !== 0 ? s.gpos(2) : s.ip + 3, 0],
     1005: [(s: State) => s.ip = s.gpos(1) !== 0 ? s.gimm(2) : s.ip + 3, 0],
     1105: [(s: State) => s.ip = s.gimm(1) !== 0 ? s.gimm(2) : s.ip + 3, 0],
     1205: [(s: State) => s.ip = s.grel(1) !== 0 ? s.gimm(2) : s.ip + 3, 0],
     2005: [(s: State) => s.ip = s.gpos(1) !== 0 ? s.grel(2) : s.ip + 3, 0],
     2105: [(s: State) => s.ip = s.gimm(1) !== 0 ? s.grel(2) : s.ip + 3, 0],
     2205: [(s: State) => s.ip = s.grel(1) !== 0 ? s.grel(2) : s.ip + 3, 0],

        6: [(s: State) => s.ip = s.gpos(1) === 0 ? s.gpos(2) : s.ip + 3, 0],
      106: [(s: State) => s.ip = s.gimm(1) === 0 ? s.gpos(2) : s.ip + 3, 0],
      206: [(s: State) => s.ip = s.grel(1) === 0 ? s.gpos(2) : s.ip + 3, 0],
     1006: [(s: State) => s.ip = s.gpos(1) === 0 ? s.gimm(2) : s.ip + 3, 0],
     1106: [(s: State) => s.ip = s.gimm(1) === 0 ? s.gimm(2) : s.ip + 3, 0],
     1206: [(s: State) => s.ip = s.grel(1) === 0 ? s.gimm(2) : s.ip + 3, 0],
     2006: [(s: State) => s.ip = s.gpos(1) === 0 ? s.grel(2) : s.ip + 3, 0],
     2106: [(s: State) => s.ip = s.gimm(1) === 0 ? s.grel(2) : s.ip + 3, 0],
     2206: [(s: State) => s.ip = s.grel(1) === 0 ? s.grel(2) : s.ip + 3, 0],

        7: [(s: State) => s.spos(3, s.gpos(1) < s.gpos(2) ? 1 : 0), 4],
      107: [(s: State) => s.spos(3, s.gimm(1) < s.gpos(2) ? 1 : 0), 4],
      207: [(s: State) => s.spos(3, s.grel(1) < s.gpos(2) ? 1 : 0), 4],
     1007: [(s: State) => s.spos(3, s.gpos(1) < s.gimm(2) ? 1 : 0), 4],
     1107: [(s: State) => s.spos(3, s.gimm(1) < s.gimm(2) ? 1 : 0), 4],
     1207: [(s: State) => s.spos(3, s.grel(1) < s.gimm(2) ? 1 : 0), 4],
     2007: [(s: State) => s.spos(3, s.gpos(1) < s.grel(2) ? 1 : 0), 4],
     2107: [(s: State) => s.spos(3, s.gimm(1) < s.grel(2) ? 1 : 0), 4],
     2207: [(s: State) => s.spos(3, s.grel(1) < s.grel(2) ? 1 : 0), 4],
    20007: [(s: State) => s.srel(3, s.gpos(1) < s.gpos(2) ? 1 : 0), 4],
    20107: [(s: State) => s.srel(3, s.gimm(1) < s.gpos(2) ? 1 : 0), 4],
    20207: [(s: State) => s.srel(3, s.grel(1) < s.gpos(2) ? 1 : 0), 4],
    21007: [(s: State) => s.srel(3, s.gpos(1) < s.gimm(2) ? 1 : 0), 4],
    21107: [(s: State) => s.srel(3, s.gimm(1) < s.gimm(2) ? 1 : 0), 4],
    21207: [(s: State) => s.srel(3, s.grel(1) < s.gimm(2) ? 1 : 0), 4],
    22007: [(s: State) => s.srel(3, s.gpos(1) < s.grel(2) ? 1 : 0), 4],
    22107: [(s: State) => s.srel(3, s.gimm(1) < s.grel(2) ? 1 : 0), 4],
    22207: [(s: State) => s.srel(3, s.grel(1) < s.grel(2) ? 1 : 0), 4],

        8: [(s: State) => s.spos(3, s.gpos(1) === s.gpos(2) ? 1 : 0), 4],
      108: [(s: State) => s.spos(3, s.gimm(1) === s.gpos(2) ? 1 : 0), 4],
      208: [(s: State) => s.spos(3, s.grel(1) === s.gpos(2) ? 1 : 0), 4],
     1008: [(s: State) => s.spos(3, s.gpos(1) === s.gimm(2) ? 1 : 0), 4],
     1108: [(s: State) => s.spos(3, s.gimm(1) === s.gimm(2) ? 1 : 0), 4],
     1208: [(s: State) => s.spos(3, s.grel(1) === s.gimm(2) ? 1 : 0), 4],
     2008: [(s: State) => s.spos(3, s.gpos(1) === s.grel(2) ? 1 : 0), 4],
     2108: [(s: State) => s.spos(3, s.gimm(1) === s.grel(2) ? 1 : 0), 4],
     2208: [(s: State) => s.spos(3, s.grel(1) === s.grel(2) ? 1 : 0), 4],
    20008: [(s: State) => s.srel(3, s.gpos(1) === s.gpos(2) ? 1 : 0), 4],
    20108: [(s: State) => s.srel(3, s.gimm(1) === s.gpos(2) ? 1 : 0), 4],
    20208: [(s: State) => s.srel(3, s.grel(1) === s.gpos(2) ? 1 : 0), 4],
    21008: [(s: State) => s.srel(3, s.gpos(1) === s.gimm(2) ? 1 : 0), 4],
    21108: [(s: State) => s.srel(3, s.gimm(1) === s.gimm(2) ? 1 : 0), 4],
    21208: [(s: State) => s.srel(3, s.grel(1) === s.gimm(2) ? 1 : 0), 4],
    22008: [(s: State) => s.srel(3, s.gpos(1) === s.grel(2) ? 1 : 0), 4],
    22108: [(s: State) => s.srel(3, s.gimm(1) === s.grel(2) ? 1 : 0), 4],
    22208: [(s: State) => s.srel(3, s.grel(1) === s.grel(2) ? 1 : 0), 4],

        9: [(s: State) => s.rb += s.gpos(1), 2],
      109: [(s: State) => s.rb += s.gimm(1), 2],
      209: [(s: State) => s.rb += s.grel(1), 2],

       99: [(s: State) => undefined as undefined, 1, Action.Hlt],
};

export class Vm {
    constructor(public id: number, code: number[]) {
        this.mem = code.map(n => ({ n }));
        this.ip = 0;
        this.rb = 0;
    }

    private mem: Mem;
    private ip: number;
    private rb: number;

    gpos = (i: number) => this.mem[this.mem[this.ip + i]?.n ?? 0]?.n ?? 0;
    gimm = (i: number) => this.mem[this.ip + i]?.n ?? 0;
    grel = (i: number) => this.mem[this.rb + (this.mem[this.ip + i]?.n ?? 0)].n ?? 0;
    spos = (i: number, n: number) => this.mem[this.mem[this.ip + i]?.n ?? 0] = { n, r: undefined as Record };
    srel = (i: number, n: number) => this.mem[this.rb + (this.mem[this.ip + i]?.n ?? 0)] = { n, r: undefined as Record };

    run = async function* (ins: AsyncGenerator<number> = (async function* () {})()): AsyncGenerator<number> {
        while (true) {
            const oc = this.mem[this.ip];
            if (oc.r === undefined) oc.r = ops[oc.n];
            const [fn, inc, act] = oc.r;

            const prm = act == Action.In ? (await ins.next()).value : undefined;
            const res = await fn(this, prm);

            if (act === Action.Out) yield res;
            else if (act === Action.Hlt) return;
            this.ip += inc;
        }
    };
}
