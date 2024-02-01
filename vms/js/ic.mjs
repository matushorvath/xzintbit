import fs from 'fs/promises';

let mem;

let ip = 0;
let rb = 0;

let monitor;
const monitorFps = 10;

const initMonitor = (imageName, imageSize) => {
    if (process.env.ICVM_MONITOR_URL) {
        monitor = {
            url: process.env.ICVM_MONITOR_URL,
            time: Date.now(),
            image: {
                name: imageName,
                size: imageSize
            },
            events: {}
        }
    }
};

const sendMonitor = async (force = false) => {
    const now = Date.now();
    if (monitor && (force || now - monitor.time >= 1000 / monitorFps)) {
        const update = {
            ip, rb,
            time: now,
            size: mem.length,
            stack: monitor.stack,
            image: monitor.image,
            events: monitor.events
        };

        monitor.events = {};
        monitor.time = now;

        try {
            await fetch(monitor.url, {
                method: 'POST',
                headers: { 'content-type': 'application/json' },
                body: JSON.stringify(update)
            });
        } catch (e) {
            // The monitor is accessed on best-effort basis, so we ignore any errors here
        }
    }
};

const getMem = (addr) => {
    if (monitor && monitor.events[addr] === undefined) {
        monitor.events[addr] = 'r';
    }
    return mem[addr] ?? 0;
};

const setMem = (addr, val) => {
    if (monitor && monitor.events[addr] !== 'w') {
        monitor.events[addr] = 'w';
    }
    mem[addr] = val;
};

const MODE_MUL = [100, 1000, 10000];

const getParam = (idx) => {
    const mode = Math.floor(getMem(ip) / MODE_MUL[idx]) % 10;
    switch (mode) {
        case 0: // position mode
            return getMem(getMem(ip + idx + 1));
        case 1: // immediate mode
            return getMem(ip + idx + 1);
        case 2: // relative mode
            return getMem(rb + getMem(ip + idx + 1));
        default:
            throw new Error(`mode error: ip ${ip} idx ${idx}`);
    }
}

const setParam = (idx, val) => {
    const mode = Math.floor(getMem(ip) / MODE_MUL[idx]) % 10;
    switch (mode) {
        case 0: // position mode
            setMem(getMem(ip + idx + 1), val);
            break;
        case 2: // relative mode
            setMem(rb + getMem(ip + idx + 1), val);
            break;
        default:
            throw new Error(`mode error: ip ${ip} idx ${idx}`);
    }
}

const run = async function* (ins = (async function* () {})()) {
    while (true) {
        await sendMonitor();

        const oc = Math.floor(getMem(ip) % 100);

        switch (oc) {
            case 1: // add
                setParam(2, getParam(0) + getParam(1));
                ip += 4;
                break;
            case 2: // mul
                setParam(2, getParam(0) * getParam(1));
                ip += 4;
                break;
            case 3: { // in
                const { value, done } = await ins.next();
                if (done) {
                    throw new Error('no more inputs');
                }
                setParam(0, value);
                ip += 2;
                break;
            }
            case 4: { // out
                const value = getParam(0);
                ip += 2;
                yield value;
                break;
            }
            case 5: // jnz
                if (getParam(0) !== 0) {
                    ip = getParam(1);
                } else {
                    ip += 3;
                }
                break;
            case 6: // jz
                if (getParam(0) === 0) {
                    ip = getParam(1);
                } else {
                    ip += 3;
                }
                break;
            case 7: // lt
                setParam(2, getParam(0) < getParam(1) ? 1 : 0);
                ip += 4;
                break;
            case 8: // eq
                setParam(2, getParam(0) === getParam(1) ? 1 : 0);
                ip += 4;
                break;
            case 9: // arb
                rb += getParam(0);
                ip += 2;
                if (monitor && monitor.stack === undefined) {
                    // Assume first arb is setting up stack
                    monitor.stack = rb;
                }
                break;
            case 99: // hlt
                return;
            default:
                throw new Error(`opcode error: ip ${ip} oc ${oc}`);
        }
    }
}

async function* getIns() {
    for await (const chunk of process.stdin) {
        for (const char of chunk) {
            yield char;
        }
    }
}

const main = async () => {
    const input = await fs.readFile(process.argv[2], 'utf8');
    mem = input.split(',').map(i => Number(i));

    initMonitor(process.argv[2], mem.length);

    for await (const char of run(getIns())) {
        process.stdout.write(String.fromCharCode(char));
    }

    await sendMonitor(true);
};

await main();
