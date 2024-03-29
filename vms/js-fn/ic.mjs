import fs from 'fs';
import fsp from 'fs/promises';

// Node.js does not have tail call optimization, so we can't use recursion instead of loops
const whilef = async (f, c, s) => {
    while (c(s)) s = await f(s);
    return s;
}

const main = async (path) => String.fromCharCode(...(await run({ mem: await load(path), ip: 0, rb: 0, out: [] })));
const load = async (path) => (await fsp.readFile(path, 'utf8')).split(',').map(n => parseInt(n, 10));
const decode = (s) => ({ 1: add, 2: mul, 3: inn, 4: out, 5: jnz, 6: jz, 7: lt, 8: eq, 9: arb, 99: hlt })[getmem(s.mem, s.ip) % 100];
const run = async (s) => (await whilef(async s => await decode(s)(s), s => !s.stop, s)).out;

const add = async (s) => ({ ...s, mem: setparam(2, s, getparam(0, s) + getparam(1, s)), ip: s.ip + 4 });
const mul = async (s) => ({ ...s, mem: setparam(2, s, getparam(0, s) * getparam(1, s)), ip: s.ip + 4 });
const inn = async (s) => (v => ({ ...s, mem: setparam(0, s, v), ip: s.ip + 2, stop: v === undefined }))(await getinput());
const out = async (s) => ({ ...s, out: [...s.out, getparam(0, s)], ip: s.ip + 2 });
const jnz = async (s) => ({ ...s, ip: getparam(0, s) !== 0 ? getparam(1, s) : s.ip + 3 });
const jz  = async (s) => ({ ...s, ip: getparam(0, s) === 0 ? getparam(1, s) : s.ip + 3 });
const lt  = async (s) => ({ ...s, mem: setparam(2, s, getparam(0, s) < getparam(1, s) ? 1 : 0), ip: s.ip + 4 });
const eq  = async (s) => ({ ...s, mem: setparam(2, s, getparam(0, s) === getparam(1, s) ? 1 : 0), ip: s.ip + 4 });
const arb = async (s) => ({ ...s, rb: s.rb + getparam(0, s), ip: s.ip + 2 })
const hlt = async (s) => ({ ...s, stop: true });

const getparam = (i, s) => [getparamp, getparami, getparamr][Math.trunc(getmem(s.mem, s.ip) / (10 ** (i + 2))) % 10](i, s);
const getparamp = (i, s) => getmem(s.mem, getmem(s.mem, s.ip + i + 1));
const getparami = (i, s) => getmem(s.mem, s.ip + i + 1);
const getparamr = (i, s) => getmem(s.mem, s.rb + getmem(s.mem, s.ip + i + 1));

const setparam = (i, s, v) => [setparamp, , setparamr][Math.trunc(getmem(s.mem, s.ip) / (10 ** (i + 2))) % 10](i, s, v);
const setparamp = (i, s, v) => setmem(s.mem, getmem(s.mem, s.ip + i + 1), v);
const setparamr = (i, s, v) => setmem(s.mem, s.rb + getmem(s.mem, s.ip + i + 1), v);

const getmem = (m, a) => m[a] ?? 0;
const setmem = (m, a, v) => ({ ...m, [a]: v });

const getinput = async () => new Promise(resolve => fs.read(
    process.stdin.fd, { buffer: Buffer.alloc(1) }, (e, c, b) => resolve(c ? b[0] : undefined)));

process.stdout.write(await main(process.argv[2]));
