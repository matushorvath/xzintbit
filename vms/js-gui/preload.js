/* eslint-env node, browser */
'use strict';

const { contextBridge, ipcRenderer } = require('electron/renderer');

contextBridge.exposeInMainWorld('vm', {
    onLoadImage: callback => ipcRenderer.on('load-image', (_event, value) => callback(value)),
    getUpdate: () => ipcRenderer.invoke('get-update')
});
