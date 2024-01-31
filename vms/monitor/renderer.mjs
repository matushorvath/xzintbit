/* eslint-env browser */

const state = {
    memSize: 0,
    imageSize: 0,
    mem: {}
};

const onUpdateState = (update) => {
    state.mem = {};

    for (const event of update) {
        if (event.e === 's') {
            // Reset
            state.memSize = event.a;
            state.imageSize = event.a;
        } else if (event.e === 'r' || event.e === 'w') {
            // Memory read/write
            state.memSize = Math.max(state.memSize, event.a);
            state.mem[event.a] = event;
        }
    };

    redraw();
};

window.ipc.onUpdateState(onUpdateState);

function setup() {
    createCanvas(1000, 800);
    background(255);
    noLoop();
}

const COLS = 200;
const SIZE = 5;

const getFill = (r, c) => {
    switch (state?.mem[r * COLS + c]?.e) {
        case undefined: return 'white';
        case 'r': return 'lightgreen';
        case 'w': return 'orange';
    }
};

const getStroke = (r, c) => {
    if (r * COLS + c < state?.imageSize) {
        return 'lightblue';
    } else {
        return 'lightgray';
    }
};

function draw() {
    clear();

    const rows = Math.max(Math.ceil(state.memSize / COLS), 100);

    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < COLS; c++) {
            stroke(getStroke(r, c));
            fill(getFill(r, c));

            square(c * SIZE, r * SIZE, SIZE);
        }
    }
}
