/* eslint-env node */

// ELECTRON_ENABLE_LOGGING=1 npm start
// ICVM_TYPE=js-gui make

// TODO show vm command line, ideally also parameters to the IC program
// TODO show stack (at least location and size), show rb register
// TODO show in/out instructions
// TODO show symbols in the memory map

import { app, BrowserWindow, ipcMain } from 'electron/main';
import zmq from 'zeromq';
import { Vm } from './vm.mjs';

import path from 'node:path';
import url from 'node:url';
import fs from 'node:fs/promises';

const __dirname = url.fileURLToPath(new URL('.', import.meta.url));

const vm = new Vm();

const handleGetUpdate = () => {
    return vm.receiveUpdate();
};

const onElectronReady = async () => {
    ipcMain.handle('get-update', handleGetUpdate);

    const win = new BrowserWindow({
        width: 1500,
        height: 900,
        webPreferences: {
            preload: path.join(__dirname, 'preload.js')
        }
    });

    await win.loadFile('index.html');
    return win;
};

const sock = new zmq.Reply();

const initZeroMQ = async (win) => {
    await sock.bind('tcp://*:2019');

    for await (const [request] of sock) {
        const data = JSON.parse(request.toString('utf8'));
        if (data.type !== 'exec') {
            await sock.send(JSON.stringify({ success: false, message: `Unexpected message type ${data.type}, expecting 'exec'` }));
            continue;
        }

        const result = await execute(win, data.path);
        await sock.send(JSON.stringify(result));
    }
};

async function* getIns() {
    while (true) {
        await sock.send(JSON.stringify({ type: 'in' }));

        const request = await sock.receive();
        const data = JSON.parse(request.toString('utf8'));

        if (data.type !== 'in') {
            throw new Error(`Unexpected message type ${data.type}, expecting 'in'`);
        }
        if (data.char === undefined) {
            throw new Error('no more inputs');
        }

        yield data.char;
    }
}

const execute = async (win, path) => {
    console.info('execute', path);

    const input = await fs.readFile(path, 'utf8');
    const mem = input.split(',').map(i => Number(i));

    const image = {
        name: path,
        size: mem.length
    };
    win.webContents.send('load-image', image);

    try {
        for await (const char of vm.run(mem, getIns())) {
            const response = { type: 'out', char: String.fromCharCode(char) };
            await sock.send(JSON.stringify(response));

            const request = await sock.receive();
            const data = JSON.parse(request.toString('utf8'));

            if (data.type !== 'out') {
                throw new Error(`Unexpected message type ${data.type}, expecting 'out'`);
            }
        }
    } catch (error) {
        return { type: 'exec', success: false, message: error.toString() };
    }

    return { type: 'exec', success: true };
};

app.whenReady()
    .then(() => onElectronReady())
    .then((win) => initZeroMQ(win));
