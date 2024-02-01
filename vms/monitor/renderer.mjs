/* eslint-env browser */

let state;

const onUpdateState = (update) => {
    state = update;
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
    const addr = r * COLS + c;

    switch (state?.events[addr]) {
        case undefined: return 'white';
        case 'r': return 'lightgreen';
        case 'w': return 'orange';
    }
};

const getStroke = (r, c) => {
    const addr = r * COLS + c;

    if (addr >= state?.rb && addr <= state?.stack) {
        return 'pink';
    } else if (addr < state?.image.size) {
        return 'lightblue';
    } else {
        return 'lightgray';
    }
};

const addrToRowCol = (addr) => [Math.floor(addr / COLS), addr % COLS];

const squareForRowCol = (r, c) => square(SIZE + c * SIZE, SIZE + r * SIZE, SIZE);

function draw() {
    clear();

    const rows = state?.size ? Math.ceil(state.size / COLS) : 100;

    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < COLS; c++) {
            stroke(getStroke(r, c));
            fill(getFill(r, c));
            squareForRowCol(r, c);
        }
    }

    const [ipr, ipc] = addrToRowCol(state?.ip ?? 0);
    noFill();
    stroke('red');
    squareForRowCol(ipr, ipc);
}
