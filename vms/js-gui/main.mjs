/* eslint-env node */

// TODO show vm command line, ideally also parameters to the IC program
// TODO show stack (at least location and size), show rb register
// TODO show in/out instructions
// TODO show symbols in the memory map

// ELECTRON_ENABLE_LOGGING=1 ICVM_TYPE=js-gui make

import { app, BrowserWindow, ipcMain } from 'electron/main';
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

const onExecute = async (win, path) => {
    const input = await fs.readFile(path, 'utf8');
    const mem = input.split(',').map(i => Number(i));

    const image = {
        name: path,
        size: mem.length
    };
    win.webContents.send('load-image', image);

    async function* getIns() {
        for await (const chunk of process.stdin) {
            for (const char of chunk) {
                yield char;
            }
        }
    }

    try {
        for await (const char of vm.run(mem, getIns())) {
            process.stdout.write(String.fromCharCode(char));
        }
    } catch (error) {
        console.log(error);
        app.exit(1);
    }

    app.quit();
};

app.whenReady()
    .then(() => onElectronReady())
    .then((win) => onExecute(win, process.argv[2]));
