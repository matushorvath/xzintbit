/* eslint-env node */

import zmq from 'zeromq';
import path from 'node:path';
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
    const url = process.env.ICVM_GUI_URL ?? 'tcp://localhost:2019';
    sock.connect(url);

    const imagePath = path.resolve(process.argv[2]);
    const input = await fs.readFile(imagePath, 'utf8');
    const image = input.split(',').map(i => Number(i));

    const request = { type: 'exec', path: imagePath, image };
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
            process.stdout.write(data.chars);
            await sock.send(JSON.stringify({ type: 'out' }));
        } else if (data.type === 'exec') {
            if (!data.success) {
                process.stderr.write(data.message);
                process.exit(1);
            }
            return;
        }
    }
};

await main();
