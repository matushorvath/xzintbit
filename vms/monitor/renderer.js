/* eslint-env browser */

const stateElement = document.getElementById('state');

const onUpdateState = (update) => {
    'use strict';

    console.log(update);
    stateElement.innerText = update;
};

window.ipc.onUpdateState(onUpdateState);
console.log('registered');
