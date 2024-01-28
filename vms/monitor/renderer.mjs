/* eslint-env browser */

const stateElement = document.getElementById('state');

const onUpdateState = (update) => {
    console.log(update);
    stateElement.innerText = update;
};

window.ipc.onUpdateState(onUpdateState);
console.log('registered');
