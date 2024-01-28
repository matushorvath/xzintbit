/* eslint-env node */
'use strict';

const { app, BrowserWindow } = require('electron');
const path = require('node:path');
const timers = require('node:timers/promises');

const createWindow = () => {
    const win = new BrowserWindow({
        width: 800,
        height: 600, webPreferences: {
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

    const win = createWindow();

    let counter = 0;
    while (true) {
        await timers.setTimeout(1000);
        updateState(win, counter++);
    }
};

main().catch(e => console.log(e));
