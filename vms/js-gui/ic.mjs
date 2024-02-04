/* eslint-env node */

import zmq from 'zeromq';
import fs from 'node:fs/promises';

async function* getIns() {
    for await (const chunk of process.stdin) {
        for (const char of chunk) {
            yield char;
        }
    }
}

const main = async () => {
    const sock = new zmq.Request();
    sock.connect('tcp://localhost:2019');

    const request = {
        type: 'exec',
        path: process.argv[2]
    };
    await sock.send(JSON.stringify(request));

    const ins = getIns();

    while (true) {
        const [result] = await sock.receive();
        const data = JSON.parse(result.toString('utf8'));

        if (data.type === 'in') {
            const { value, done } = await ins.next();
            if (done) {
                await sock.send(JSON.stringify({ type: 'in' }));
            } else {
                await sock.send(JSON.stringify({ type: 'in', char: value }));
            }
        } else if (data.type === 'out') {
            process.stdout.write(data.char);
            await sock.send(JSON.stringify({ type: 'out' }));
        } else if (data.type === 'exec') {
            if (!data.success) {
                process.stdout.write(data.message);
                process.exit(1);
            }
            return;
        }
    }
};

await main();
