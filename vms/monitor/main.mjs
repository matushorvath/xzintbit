/* eslint-env node */

import { app, BrowserWindow } from 'electron';

import path from 'node:path';
import timers from 'node:timers/promises';
import url from 'url';

const __dirname = url.fileURLToPath(new URL('.', import.meta.url));

const createWindow = () => {
    const win = new BrowserWindow({
        width: 800,
        height: 600,
        webPreferences: {
            preload: path.join(__dirname, 'preload.js')
        }
    });

    win.loadFile('index.html');
    return win;
};

const updateState = (win, update) => {
    console.log(update);
    win.webContents.send('update-state', update);
};

const main = async () => {
    await app.whenReady();
    console.log('ready');

    const win = createWindow();

    let counter = 0;
    while (true) {
        await timers.setTimeout(1000);
        updateState(win, counter++);
    }
};

main().catch(e => {
    console.log(e);
    app.quit();
});
