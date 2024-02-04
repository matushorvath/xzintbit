/* eslint-env node */

// TODO synchronize with the VM, right now we are showing old data
// TODO show vm command line, ideally also parameters to the IC program
// TODO show stack (at least location and size), show rb register
// TODO show in/out instructions
// TODO generate symbol files, show symbols in the memory map

import { app, BrowserWindow } from 'electron';
import express from 'express';

import path from 'node:path';
import url from 'node:url';

const __dirname = url.fileURLToPath(new URL('.', import.meta.url));

const onEventReceived = async (req, res, win) => {
    //console.log(req.body);
    win?.webContents?.send('update-state', req.body);
    // TODO wait for draw to finish, send the status back
    // TODO this way we can hopefully sync the VM and the monitor
    res.status(201).send('Created');
};

const server = express();
const port = 2019;
server.use(express.json());

const onElectronReady = () => {
    const win = new BrowserWindow({
        width: 1100,
        height: 800,
        webPreferences: {
            preload: path.join(__dirname, 'preload.js')
        }
    });

    server.post('/event', (req, res) => onEventReceived(req, res, win));
    server.listen(port, () => console.log(`Listening on port ${port}`));

    // TODO race condition, we must not receive events until the browser part is ready for them
    // TODO probably handle by maintaining state in node, and exposing the state to renderer
    // TODO or synchronize the startup, make browser code send and event to us that it is ready

    win.loadFile('index.html');
    return win;
};

app.whenReady().then(() => onElectronReady());
