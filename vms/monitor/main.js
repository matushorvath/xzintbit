/* eslint-env node */
'use strict';

const { app, BrowserWindow } = require('electron');
const path = require('node:path');

const createWindow = () => {
    const win = new BrowserWindow({
        width: 800,
        height: 600, webPreferences: {
            preload: path.join(__dirname, 'preload.js')
        }
    });

    win.loadFile('index.html');
};

const main = async () => {
    await app.whenReady();
    createWindow();
};

main().catch(e => console.log(e));
