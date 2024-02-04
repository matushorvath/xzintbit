/* eslint-env node, browser */
'use strict';

const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('ipc', {
    onUpdateState: callback => ipcRenderer.on('update-state', (_event, value) => callback(value))
});
